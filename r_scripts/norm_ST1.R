library(goeveg)


###########
# summarize ST1

head(ST1[[1]])

ST5_new <- list()
for (i in 1:(length(ST1))) {

  data_201 <- ST1[[i]] %>% dplyr::filter(gen %in% c(201)) %>% select(-1) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_201 = predicted.value)
  data_202 <- ST1[[i]] %>% dplyr::filter(gen %in% c(202)) %>% select(-1) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_202 = predicted.value)
  
  data_new <- ST1[[i]] %>% dplyr::filter(!gen %in% c(201, 202)) %>% group_by(env) %>% summarise_at(vars(predicted.value), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_new <- data_new %>% mutate_at(vars(c(mean,min,max)), funs(round(., 2))) %>% mutate_at(vars(cv), funs(round(., 4))) %>% mutate_at(vars(sd), funs(round(., 3))) %>% unite("range", c(min, max), sep = " - ", remove = T)  %>% unite("mean", c(mean, sd), sep = " ±", remove = T)
  data_new <- data_new %>% inner_join(., data_201, by = "env") %>% inner_join(., data_202, by = "env") %>% remove_rownames() %>% column_to_rownames("env") %>% relocate(mean)
  class(data_new)
  data_new <- t(data_new)
  data_new <- as.data.frame(data_new) 
  data_new <- data_new %>% rownames_to_column("stat")
  
  ST5_new[[length(ST5_new)+1]] <- data_new
}

names(ST5_new) <- names(ST1)
ST5_new <-rbindlist(ST5_new, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(ST5_new, "~/Documents/Cesar/git/quality_2022/Pheno/ST5_new.csv", quote = F, row.names = F)

data_201 <- ST5_201[[1]] %>% select(-1) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_201 = predicted.value)
data_202 <- ST5_202[[1]] %>% select(-1) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_202 = predicted.value)

data_new <- ST5_new[[1]] %>% mutate_at(vars(-c(env, cv)), funs(round(., 2))) %>% mutate_at(vars(cv), funs(round(., 4))) %>% unite("range", c(min, max), sep = " - ", remove = T)  %>% unite("mean", c(mean, sd), sep = " ±", remove = T)
data_new <- data_new %>% inner_join(., data_201, by = "env") %>% inner_join(., data_202, by = "env") %>% remove_rownames() %>% column_to_rownames("env") %>% relocate(mean)
class(data_new)
data_new <- t(data_new)
data_new <- as.data.frame(data_new) 
data_new <- data_new %>% rownames_to_column("stat")


paste0("first_string", 2, "±", 3, "second_string", collapse = "")


#############
# Summarize ST2

head(ST2[[1]])

ST6_new <- list()
for (i in 1:(length(ST2))) {
  
  data_201 <- ST2[[i]] %>% dplyr::filter(gen %in% c(201)) %>% select(-1) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_201 = predicted.value)
  data_202 <- ST2[[i]] %>% dplyr::filter(gen %in% c(202)) %>% select(-1) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_202 = predicted.value)
  data_new <- ST2[[i]] %>% dplyr::filter(!gen %in% c(201, 202)) %>% group_by(loc) %>% summarise_at(vars(predicted.value), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_new <- data_new %>% mutate_at(vars(c(mean,min,max)), funs(round(., 2))) %>% mutate_at(vars(cv), funs(round(., 4))) %>% mutate_at(vars(sd), funs(round(., 3))) %>% unite("range", c(min, max), sep = " - ", remove = T)  %>% unite("mean", c(mean, sd), sep = " ±", remove = T)
  data_new <- data_new %>% inner_join(., data_201, by = "loc") %>% inner_join(., data_202, by = "loc") %>% remove_rownames() %>% column_to_rownames("loc") %>% relocate(mean)
  data_new <- t(data_new)
  data_new <- as.data.frame(data_new) 
  data_new <- data_new %>% rownames_to_column("stat")
  
  ST6_new[[length(ST6_new)+1]] <- data_new
}
names(ST6_new) <- names(ST2)
ST6_new <-rbindlist(ST6_new, use.names=TRUE, fill=TRUE, idcol="trait")


# Summarize ST3

head(ST3[[1]])

ST3.1 <-rbindlist(ST3, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST3.1)

data_201 <- ST3.1 %>% dplyr::filter(gen %in% c(201)) %>% select(-2) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_201 = predicted.value)
data_202 <- ST3.1 %>% dplyr::filter(gen %in% c(202)) %>% select(-2) %>% mutate_at(vars(predicted.value), funs(round(., 2))) %>% rename(Check_202 = predicted.value)
data_new <- ST3.1 %>% dplyr::filter(!gen %in% c(201, 202)) %>% group_by(trait) %>% summarise_at(vars(predicted.value), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
data_new <- data_new %>% mutate_at(vars(c(mean,min,max)), funs(round(., 2))) %>% mutate_at(vars(cv), funs(round(., 4))) %>% mutate_at(vars(sd), funs(round(., 3))) %>% unite("range", c(min, max), sep = " - ", remove = T)  %>% unite("mean", c(mean, sd), sep = " ±", remove = T)
data_new <- data_new %>% inner_join(., data_201, by = "trait") %>% inner_join(., data_202, by = "trait") %>% remove_rownames() %>% column_to_rownames("trait") %>% relocate(mean)

ST7_new <- data_new %>% rownames_to_column("trait")
ST7_new <- ST7_new %>% gather(key = "stat", value = "overall", 2:6)

ST7_new <- inner_join(ST6_new, ST7_new, by = c("trait", "stat"))
write.csv(ST7_new, "~/Documents/Cesar/git/quality_2022/Pheno/ST7_new.csv", quote = F, row.names = F)


#############
# tables
head(ST1[[1]])
head(ST1.1)
ST1.1 <-rbindlist(ST1, use.names=TRUE, fill=TRUE, idcol="trait")

T1 <- ST1.1%>% unite("trait", c(trait, env), sep = "_", remove = T) %>% spread(key = trait, value = 3)

ST2.1 <-rbindlist(ST2, use.names=TRUE, fill=TRUE, idcol="trait") %>% unite("env", c(trait, loc), sep = "_", remove = T) %>% spread(key = env, value = 3)
head(ST2.1)

ST3.1 <-rbindlist(ST3, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST3.1)
ST3.1 <- ST3.1 %>% spread(key = trait, value = 3)

write.csv(T1, "~/Documents/Cesar/git/quality_2022/Pheno/ST1.csv", quote = F, row.names = F)
write.csv(ST2.1, "~/Documents/Cesar/git/quality_2022/Pheno/ST2.csv", quote = F, row.names = F)
write.csv(ST3.1, "~/Documents/Cesar/git/quality_2022/Pheno/ST3.csv", quote = F, row.names = F)
