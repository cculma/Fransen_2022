head(ST0)

dat1 <- ST0 %>% select(1:3) %>% separate(1, c("loc", "year", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year), sep = "_", remove = T) 
head(dat1)

dat1 <- split(dat1[,-2], dat1$trait)

###########
# summarize ST1

head(dat1[[1]])

ST0_new <- list()
for (i in 1:(length(dat1))) {
  
  data_201 <- dat1[[i]] %>% dplyr::filter(gen %in% c(201)) %>% select(-2) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_201 = predicted.value)
  data_202 <- dat1[[i]] %>% dplyr::filter(gen %in% c(202)) %>% select(-2) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_202 = predicted.value)
  
  data_new <- dat1[[i]] %>% dplyr::filter(!gen %in% c(201, 202)) %>% group_by(env) %>% summarise_at(vars(predicted.value), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_new <- data_new %>% mutate_at(vars(c(mean,min,max)), funs(round(., 2))) %>% mutate_at(vars(cv), funs(round(., 4))) %>% mutate_at(vars(sd), funs(round(., 3))) %>% unite("range", c(min, max), sep = " - ", remove = T)  %>% unite("mean", c(mean, sd), sep = " ±", remove = T)
  data_new <- data_new %>% inner_join(., data_201, by = "env") %>% inner_join(., data_202, by = "env") %>% remove_rownames() %>% column_to_rownames("env") %>% relocate(mean)
  data_new <- t(data_new)
  data_new <- as.data.frame(data_new) 
  data_new <- data_new %>% rownames_to_column("stat")
  
  ST0_new[[length(ST0_new)+1]] <- data_new
}

names(ST0_new) <- names(dat1)
ST0_new <-rbindlist(ST0_new, use.names=TRUE, fill=TRUE, idcol="trait")

write.csv(ST0_new, "~/Documents/Cesar/git/quality_2022/Pheno/ST0_new.csv", quote = F, row.names = F)

##############
# RAW
setwd("~/Documents/Cesar/git/quality_2022/Pheno/")

a1 <- read.csv("pheno1.csv")
b1 <- read.csv("cols_rows1.csv")
head(a1)
head(b1)
dim(a1)

lev4 <- c("loc", "block", "position",	"ID", "gen")
a2 <- left_join(b1, a1, by = lev4)
head(a2)
dim(a2)
colnames(a2)
a3 <- a2 %>% gather(key = "trait", value = "raw", 9:24)
head(a3)

lev5 <- colnames(a3)[-10]
a3[,lev5] <- lapply(a3[,lev5], factor)
str(a3)
a3 <- a3 %>% unite("env", c(loc, year), sep = "_", remove = T)
head(a3)

new <- a3 %>% dplyr::filter(!gen %in% c(202,202))
c201 <- a3 %>% dplyr::filter(gen %in% c(201))
c202 <- a3 %>% dplyr::filter(gen %in% c(202))

new <- new[,-c(2:7)]
c201 <- c201[,-c(2:7)]
c202 <- c202[,-c(2:7)]

new <- split(new[,-2], new$trait)
c201 <- split(c201[,-2], c201$trait)
c202 <- split(c202[,-2], c202$trait)

head(c201[[1]])

RAW_new <- list()
for (i in 1:(length(new))) {
  data_new <- new[[i]] %>% group_by(env) %>% summarise_at(vars(raw), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_new <- data_new %>% mutate_at(vars(c(mean,min,max)), funs(round(., 2))) %>% mutate_at(vars(cv), funs(round(., 4))) %>% mutate_at(vars(sd), funs(round(., 3))) %>% unite("range", c(min, max), sep = " - ", remove = T)  %>% unite("mean", c(mean, sd), sep = " ±", remove = T)
  data_new <- data_new %>% column_to_rownames("env") %>% relocate(mean)
  data_new <- t(data_new)
  data_new <- as.data.frame(data_new) 
  data_new <- data_new %>% rownames_to_column("stat")
  data_new$entry <- "new"
  RAW_new[[length(RAW_new)+1]] <- data_new
}

RAW_201 <- list()
for (i in 1:(length(new))) {
  data_new <- c201[[i]] %>% group_by(env) %>% summarise_at(vars(raw), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_new <- data_new %>% mutate_at(vars(c(mean,min,max)), funs(round(., 2))) %>% mutate_at(vars(cv), funs(round(., 4))) %>% mutate_at(vars(sd), funs(round(., 3))) %>% unite("range", c(min, max), sep = " - ", remove = T)  %>% unite("mean", c(mean, sd), sep = " ±", remove = T)
  data_new <- data_new %>% column_to_rownames("env") %>% relocate(mean)
  data_new <- t(data_new)
  data_new <- as.data.frame(data_new) 
  data_new <- data_new %>% rownames_to_column("stat")
  data_new$entry <- "201"
  RAW_201[[length(RAW_201)+1]] <- data_new
}

RAW_202 <- list()
for (i in 1:(length(new))) {
  data_new <- c202[[i]] %>% group_by(env) %>% summarise_at(vars(raw), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_new <- data_new %>% mutate_at(vars(c(mean,min,max)), funs(round(., 2))) %>% mutate_at(vars(cv), funs(round(., 4))) %>% mutate_at(vars(sd), funs(round(., 3))) %>% unite("range", c(min, max), sep = " - ", remove = T)  %>% unite("mean", c(mean, sd), sep = " ±", remove = T)
  data_new <- data_new %>% column_to_rownames("env") %>% relocate(mean)
  data_new <- t(data_new)
  data_new <- as.data.frame(data_new) 
  data_new <- data_new %>% rownames_to_column("stat")
  data_new$entry <- "202"
  RAW_202[[length(RAW_202)+1]] <- data_new
}

names(RAW_new) <- names(new)
names(RAW_201) <- names(new)
names(RAW_202) <- names(new)

RAW_new <-rbindlist(RAW_new, use.names=TRUE, fill=TRUE, idcol="trait")
RAW_201 <-rbindlist(RAW_201, use.names=TRUE, fill=TRUE, idcol="trait")
RAW_202 <-rbindlist(RAW_202, use.names=TRUE, fill=TRUE, idcol="trait")

RAW1 <- rbind(RAW_new, RAW_201, RAW_202)
RAW1 <- RAW1[order(RAW1$trait), ]
colnames(RAW1)
RAW1 <- RAW1[,c(1,14,2:13)]
RAW1 <- RAW1 %>% unite("stat", c(entry, stat), sep = "_", remove = T) 
head(RAW1)

write.csv(RAW1, "~/Documents/Cesar/git/quality_2022/Pheno/RAW1.csv", quote = F, row.names = F)


