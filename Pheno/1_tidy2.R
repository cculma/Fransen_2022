rm(list = ls())
session_info()
library(tidyverse)
library(ggcorrplot)
library(patchwork)


setwd("~/Documents/Cesar/git/quality_2022/Pheno/")

a1 <- read.csv("pheno1.csv")
b1 <- read.csv("cols_rows1.csv")


colnames(a1)
lev1 <- colnames(a1)[1:6]
a1[,lev1] <- lapply(a1[,lev1], factor)
str(a1)
head(a1)


a2 <- a1 %>% select(c(1,6,7)) %>% spread(key = year, value = DM)
head(a2) 
a3 <- a2 %>% select(-5) %>% remove_rownames() %>% column_to_rownames("ID")

BLUP2.1 <- cor(a6, use = "complete")
ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "Pearson ST3")


levels(a1$gen)
a4 <- a1 %>% dplyr::filter(!gen %in% c(201, 202)) %>% select(c(4:6,8)) %>% unite("env", c(loc, year), sep = "_", remove = T) 


a5 <- a1 %>% dplyr::filter(gen %in% c(201, 202)) %>% select(c(2,4:6,8)) %>% unite("env", c(loc, year), sep = "_", remove = T) %>% unite("gen", c(gen, block), sep = "_", remove = T)   

a6 <- rbind(a4, a5)
a6 <- a6 %>% spread(key = env, value = CP) %>% remove_rownames() %>% column_to_rownames("gen")

lev2 <- colnames(a1)[7:22]
seq(7:22)

BLUP2.1 <- cor(a6, use = "complete")
ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = lev2[2])


cor1 <- list() 
for (i in 7:ncol(a1)) {
  print(i)
  a4 <- a1 %>% dplyr::filter(!gen %in% c(201, 202)) %>% select(c(4:6,all_of(i))) %>% unite("env", c(loc, year), sep = "_", remove = T)
  a5 <- a1 %>% dplyr::filter(gen %in% c(201, 202)) %>% select(c(2,4:6,all_of(i))) %>% unite("env", c(loc, year), sep = "_", remove = T) %>% unite("gen", c(gen, block), sep = "_", remove = T)
  a6 <- rbind(a4, a5)
  print(head(a6))
  lev3 <- colnames(a6)[3]
  a6 <- a6 %>% spread(key = env, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
  BLUP2.1 <- cor(a6, use = "complete")
  g1 <- ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(legend.position = "none" ,axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = paste(lev3, "raw"))
  cor1[[length(cor1)+1]] <-  g1
}


cor1[[1]] + cor1[[2]] + cor1[[3]] + cor1[[4]] + plot_layout(ncol = 2)
cor1[[5]] + cor1[[6]] + cor1[[7]] + cor1[[12]] + plot_layout(ncol = 2)
cor1[[8]] + cor1[[9]] + cor1[[10]] + cor1[[11]] + plot_layout(ncol = 2)
cor1[[13]] + cor1[[14]] + cor1[[15]] + cor1[[16]] + plot_layout(ncol = 2)

head(a1)
a8 <- a1 %>% select(-c(1:3)) %>% unite("env", c(loc, year), sep = "_", remove = T) %>%  gather(key = "trait", value = "raw", 3:18)
head(a8)
a8 <- na.omit(a8)

ggplot(a8, aes(x = env, y = raw)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + facet_wrap(~trait, ncol = 4, scales = "free_y") + labs(y = "Raw values", x = "") 

a9 <- a8 %>% dplyr::filter(trait %in% c("CP"))
raw_CP <- ggplot(a9, aes(x = env, y = raw)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + labs(title = "CP Raw", y = "", x = "") 

