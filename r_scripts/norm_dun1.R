rm(list = ls())

# quality
library(data.table)
library(tidyverse)
library(ggpubr)
library(StageWise)
library(asreml)
library(asremlPlus)
library(stringr)
library(PMCMRplus)


head(ST1[[1]])
ST1.1 <-rbindlist(ST1, use.names=TRUE, fill=TRUE, idcol="trait") %>% unite("trait", c(trait, env), sep = "_", remove = T)

ST1.1 <- split(ST1.1[,-1], ST1.1$trait)
head(ST1.1[[1]])
#################
# Scaling
SC1 <- list()
for (i in 1:(length(ST1.1))) {
  data <- ST1.1[[i]]
  data$scaled <- as.data.frame(scale(data$predicted.value))
  data <- data[,-2]
  SC1[[length(SC1)+1]] <- data
}
names(SC1) <- names(ST1.1)
SC1 <- rbindlist(SC1, use.names=TRUE, fill=TRUE, idcol="trait")

SC1 <- SC1 %>% separate(1, c("trait", "loc", "year"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year), sep = "_", remove = T)

head(SC1)
SC1 <- split(SC1[,-1], SC1$trait)
names(SC1)

##########
# bloxplot

box4 <- list()
for (i in 1:length(SC1)) {
  a6 <- SC1[[1]] %>% separate(1, c("loc", "year"), sep = "_", remove = F, convert = FALSE, extra = "merge")
  lev3 <- names(SC1)[i]
  g1 <- ggplot(a6, aes(x = env, y = scaled)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + labs(title = paste(lev3, "normalized"), y = "", x = "")
  box4[[length(box4)+1]] <-  g1
}
names(box4) <- names(SC1)

box4[[7]] + box4[[6]] + box4[[1]] + box4[[3]] + plot_layout(ncol = 2)
box4[[4]] + box4[[9]] + box4[[11]] + box4[[15]] + plot_layout(ncol = 2)
box4[[5]] + box4[[14]] + box4[[10]] + box4[[12]] + plot_layout(ncol = 2)
box4[[8]] + box4[[16]] + box4[[2]] + box4[[13]] + plot_layout(ncol = 2)


box1[[6]] + box4[[6]] + plot_layout(ncol = 1)


################
# dunnet

a3 <- SC1[[1]]
head(a3)
D201 <- a3 %>% dplyr::filter(gen %in% c(201))
D202 <- a3 %>% dplyr::filter(gen %in% c(202))
D3 <- a3 %>% dplyr::filter(!gen %in% c(201,202))
D3$gen <- gsub("^", "N_", D3$gen)
D201$gen <- gsub("^", "C_", D201$gen)
D202$gen <- gsub("^", "C_", D202$gen)

D4 <- rbind(D201, D3)
D5 <- rbind(D202, D3)
lev5 <- c("env","gen")
D4 <- as.data.frame(D4)
D5 <- as.data.frame(D5)
D4[,lev5] <- lapply(D4[,lev5], factor)
D5[,lev5] <- lapply(D5[,lev5], factor)

model_201 <- aov(scaled ~ gen, data = D4)
model_202 <- aov(scaled ~ gen, data = D5)
summary(model_201)
summary(model_202)

D6 <- summary(dunnettTest(model_201))
D7 <- as.data.frame(D6[["p.value"]])
colnames(D7) <- paste(colnames(D7), names(qual_BLUP2[i]), names(B1[j]), sep = "_", collapse = NULL)
D8 <- summary(dunnettTest(model_202))
D9 <- as.data.frame(D8[["p.value"]])
colnames(D9) <- paste(colnames(D9), names(qual_BLUP2[i]), names(B1[j]), sep = "_", collapse = NULL)

Dun_201[[length(Dun_201)+1]] <- D7
Dun_202[[length(Dun_202)+1]] <- D9



names(SC1)
Dun_201.1 <- list()
Dun_202.1 <- list()

for (i in 1:(length(SC1))) {
  D1 <- as.data.frame(SC1[[i]])
  D201 <- D1 %>% dplyr::filter(gen %in% c(201))
  D202 <- D1 %>% dplyr::filter(gen %in% c(202))
  D3 <- D1 %>% dplyr::filter(!gen %in% c(201,202))
  D3$gen <- gsub("^", "N_", D3$gen)
  D201$gen <- gsub("^", "C_", D201$gen)
  D202$gen <- gsub("^", "C_", D202$gen)
  
  D4 <- rbind(D201, D3)
  D5 <- rbind(D202, D3)
  D4$gen <- as.factor(D4$gen)
  D5$gen <- as.factor(D5$gen)
  
  # D4[,lev5] <- lapply(D4[,lev5], factor)
  # D5[,lev5] <- lapply(D5[,lev5], factor)
  
  D7 <- DunnettTest(x=D4$scaled, g=D4$gen)
  D9 <- DunnettTest(x=D5$scaled, g=D5$gen)
  D7 <- as.data.frame(D7[[1]])
  D9 <- as.data.frame(D9[[1]])
  
  Dun_201.1[[length(Dun_201.1)+1]] <- D7
  Dun_202.1[[length(Dun_202.1)+1]] <- D9
}

names(Dun_201.1) <- names(SC1)
names(Dun_202.1) <- names(SC1)



######
# Scaling ST2

ST2.1 <-rbindlist(ST2, use.names=TRUE, fill=TRUE, idcol="trait") %>% unite("env", c(trait, loc), sep = "_", remove = T) 
head(ST2.1)
ST2.1 <- split(ST2.1[,-1], ST2.1$env)
names(ST2.1)

#################
# Scaling
SC1 <- list()
for (i in 1:(length(ST2.1))) {
  data <- ST2.1[[i]]
  data$scaled <- as.data.frame(scale(data$predicted.value))
  data <- data[,-2]
  SC1[[length(SC1)+1]] <- data
}
names(SC1) <- names(ST2.1)
SC1 <- rbindlist(SC1, use.names=TRUE, fill=TRUE, idcol="trait")
SC1 <- SC1 %>% separate(1, c("trait", "loc"), sep = "_", remove = T, convert = FALSE, extra = "merge") 
head(SC1)
SC1 <- split(SC1[,-1], SC1$trait)
names(SC1)
#################
# spread data in 

names(SC1)
c1 <- c(1:16)
c1[-c(list2)]

list2 <- c(1,2,3,7,10,11) # c(11,7,1,3,10,2) # 201 > 202 
list3 <- c1[-c(list2)] # 201 < 202

names(SC1)
SC2 <- SC1[list2] # HisB
SC3 <- SC1[list3] # LisB
names(SC2) # 201 > 202
names(SC3) # 201 < 202

hist(SC2[[1]]$scaled)

##############

names(SC3) # 201 < 202
SC4 <- list()
for (i in 1:length(SC3)) {
  a3 <- as.data.frame(SC3[[i]])
  head(a3)
  D201 <- a3 %>% dplyr::filter(gen %in% c(201))
  D202 <- a3 %>% dplyr::filter(gen %in% c(202))
  D3 <- a3 %>% dplyr::filter(!gen %in% c(201,202))
  D3$gen <- gsub("^", "N_", D3$gen)
  D201$gen <- gsub("^", "C_", D201$gen)
  D202$gen <- gsub("^", "C_", D202$gen)
  
  D4 <- rbind(D201, D3)
  D5 <- rbind(D202, D3)
  lev5 <- c("env","gen")
  D4[,lev5] <- lapply(D4[,lev5], factor)
  D5[,lev5] <- lapply(D5[,lev5], factor)
  model_201 <- aov(scaled ~ gen, data = D4)
  model_202 <- aov(scaled ~ gen, data = D5)
  
  D6 <- dunnettTest(model_201, alternative = "less")
  D7 <- as.data.frame(D6[["p.value"]])
  D8 <- dunnettTest(model_202, alternative = "greater")
  D9 <- as.data.frame(D8[["p.value"]])
  
  E1 <- merge(D7, D9, by=0, all=TRUE) %>% separate(1, into = c("N", "gen"),sep = "_") %>% select(-1)
  SC4[[length(SC4)+1]] <- E1
}

names(SC4) <- names(SC3)
SC4 <- rbindlist(SC4, use.names=TRUE, fill=TRUE, idcol="trait")
SC4$trait <- as.factor(SC4$trait)
levels(SC4$trait)
head(SC4)
SC4_201 <- SC4[,-4] %>% dplyr::filter(C_201 < 0.05) %>% spread(key = trait, value = C_201)
SC4_202 <- SC4[,-3] %>% dplyr::filter(C_202 < 0.05) %>% spread(key = trait, value = C_202)

#~~~~~~~~~~~~~~~~

names(SC2) # 201 > 202
SC5 <- list()
for (i in 1:length(SC2)) {
  a3 <- as.data.frame(SC2[[i]])
  head(a3)
  D201 <- a3 %>% dplyr::filter(gen %in% c(201))
  D202 <- a3 %>% dplyr::filter(gen %in% c(202))
  D3 <- a3 %>% dplyr::filter(!gen %in% c(201,202))
  D3$gen <- gsub("^", "N_", D3$gen)
  D201$gen <- gsub("^", "C_", D201$gen)
  D202$gen <- gsub("^", "C_", D202$gen)
  
  D4 <- rbind(D201, D3)
  D5 <- rbind(D202, D3)
  lev5 <- c("env","gen")
  D4[,lev5] <- lapply(D4[,lev5], factor)
  D5[,lev5] <- lapply(D5[,lev5], factor)
  model_201 <- aov(scaled ~ gen, data = D4)
  model_202 <- aov(scaled ~ gen, data = D5)
  
  D6 <- dunnettTest(model_201, alternative = "greater")
  D7 <- as.data.frame(D6[["p.value"]])
  D8 <- dunnettTest(model_202, alternative = "less")
  D9 <- as.data.frame(D8[["p.value"]])
  E1 <- merge(D7, D9, by=0, all=TRUE) %>% separate(1, into = c("N", "gen"),sep = "_") %>% select(-1)
  SC5[[length(SC5)+1]] <- E1
}
names(SC5) <- names(SC2)
SC5 <- rbindlist(SC5, use.names=TRUE, fill=TRUE, idcol="trait")
SC5$trait <- as.factor(SC5$trait)
levels(SC5$trait)

SC5_201 <- SC5[,-4] %>% dplyr::filter(C_201 < 0.05) %>% spread(key = trait, value = C_201)
SC5_202 <- SC5[,-3] %>% dplyr::filter(C_202 < 0.05) %>% spread(key = trait, value = C_202)


SC6 <- full_join(SC4_201, SC5_201, by = "gen")
SC7 <- full_join(SC4_202, SC5_202, by = "gen")
SC6$diff <- "diff_201"
SC7$diff <- "diff_202"
colnames(SC6)
colnames(SC7)



write.csv(SC6, "~/Documents/Cesar/git/quality_2022/Pheno/SC6.csv", quote = F, row.names = F)
write.csv(SC7, "~/Documents/Cesar/git/quality_2022/Pheno/SC7.csv", quote = F, row.names = F)

colSums(!is.na(SC6))
colSums(!is.na(SC7))

###############




a3 <- as.data.frame(SC1[[14]])
head(a3)
D201 <- a3 %>% dplyr::filter(gen %in% c(201))
D202 <- a3 %>% dplyr::filter(gen %in% c(202))
D3 <- a3 %>% dplyr::filter(!gen %in% c(201,202))
D3$gen <- gsub("^", "N_", D3$gen)
D201$gen <- gsub("^", "C_", D201$gen)
D202$gen <- gsub("^", "C_", D202$gen)

D4 <- rbind(D201, D3)
D5 <- rbind(D202, D3)
lev5 <- c("env","gen")
D4[,lev5] <- lapply(D4[,lev5], factor)
D5[,lev5] <- lapply(D5[,lev5], factor)

# D4$gen <- as.factor(D4$gen)
# D5$gen <- as.factor(D5$gen)

model_201 <- aov(scaled ~ gen, data = D4)
model_202 <- aov(scaled ~ gen, data = D5)
summary(model_201)
summary(model_202)

# HisB
D6 <- summary(dunnettTest(model_201, alternative = "less"))
D7 <- as.data.frame(D6[["p.value"]])
D8 <- summary(dunnettTest(model_202, alternative = "greater"))
D9 <- as.data.frame(D8[["p.value"]])

D7 <- dunnettTest(model_201, alternative = "less")
class(D7)
summary(D7)
D7.1 <- as.data.frame(get.pvalues(D6))
colnames(D9) <- paste(colnames(D9), names(qual_BLUP2[i]), names(B1[j]), sep = "_", collapse = NULL)

head(D7)
# Dun_201[[length(Dun_201)+1]] <- D7
# Dun_202[[length(Dun_202)+1]] <- D9


a3 <- as.data.frame(SC3[[3]])
# LisB
D6 <- dunnettTest(model_201, alternative = "greater")
D7 <- as.data.frame(D6[["p.value"]])
D8 <- dunnettTest(model_202, alternative = "less")
D9 <- as.data.frame(D8[["p.value"]])

D7.1 <- merge(D7, D9, by=0, all=TRUE) %>% separate(1, into = c("N", "gen"),sep = "_") %>% select(-1)

D7.1 <- as.data.frame(get.pvalues(D6))
D7.1 <- as.data.table(summary(D6))
print.PMCMR
barPlot(D6, alpha = 0.05)
plot(D6, alpha = 0.05)
print(D6, powerPMCMR)
