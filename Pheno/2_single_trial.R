library(tidyverse)
library(ggcorrplot)
library(patchwork)
library(asreml)
library(asremlPlus)
library(data.table)

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
head(a3)

c201 <- a3 %>% dplyr::filter(gen %in% c(201))
c202 <- a3 %>% dplyr::filter(gen %in% c(202))
head(c201)
c201 <- c201[,-c(3:7)]
colnames(c201)[5] <- "cov1"
c202 <- c202[,-c(3:7)]
colnames(c202)[5] <- "cov2"

a3 <- a3 %>% left_join(., c201, by= c("loc", "block", "year", "trait")) %>% left_join(., c202, by= c("loc", "block", "year", "trait")) %>% unite("env", c(loc, year, trait), sep = "_", remove = F)


a3$env <- as.factor(a3$env)
str(a3)
a3 <- split(a3, a3$env)
head(a3[[1]])
data <- a3[[1]]
head(data)
str(data)

comp_var <- list() 
ST0 <- list()
for (i in 1:length(a3)) {
  data <- a3[[i]]
  data <- data[order(data$row, data$col), ]

  # m3 <- asreml::asreml(fixed = raw ~ 1 + cov1 + cov2,
  #                      random = ~ idv(gen) , 
  #                      residual = ~ar1(row):ar1(col),
  #                      data = data,
  #                      na.action = list(x = "include", y = "include"))

  m3 <- asreml::asreml(fixed = raw ~ 1 + cov1 + cov2,
                       random = ~ block*gen,
                       residual=~  ar1(row):ar1(col),
                       data = data,
                       na.action = list(x = "include", y = "include"))

  # comp1 <- summary(m3)$varcomp
  
  preds <- predict(m3, classify = "gen", vcov = TRUE)
  blue <- preds$pvals
  blue <- blue[1:3]
  head(blue)
  blue$weight <- (1/blue$std.error)^2
  
  ST0[[length(ST0)+1]] <- blue
  # comp_var[[length(comp_var)+1]] <- comp1
}

names(ST0) <- names(a3)
names(comp_var) <- names(a3)

for (i in 1:(length(ST0))) {
  print(sd(ST0[[i]]$predicted.value))
  # print(max(ST0[[i]]$predicted.value))
  # print(min(ST0[[i]]$predicted.value))
}

ST0 <-rbindlist(ST0, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST0)

dat1 <- ST0 %>% select(1:3) %>% separate(1, c("loc", "year", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year), sep = "_", remove = T) 

dat1 <- split(dat1[,-2], dat1$trait, keep.by = F)

# head(dat1[[1]])

cor2 <- list() 
for (i in 1:length(dat1)) {
  a6 <- dat1[[i]] %>% spread(key = env, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
  BLUP2.1 <- cor(a6, use = "complete")
  lev3 <- names(dat1)[i]
  g1 <- ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(legend.position = "none" ,axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = paste(lev3, "ST0"))
  cor2[[length(cor2)+1]] <-  g1
}
names(dat1)

cor2[[7]] + cor2[[6]] + cor2[[1]] + cor2[[3]] + plot_layout(ncol = 2)
cor2[[4]] + cor2[[9]] + cor2[[11]] + cor2[[15]] + plot_layout(ncol = 2)
cor2[[5]] + cor2[[14]] + cor2[[10]] + cor2[[12]] + plot_layout(ncol = 2)
cor2[[8]] + cor2[[16]] + cor2[[2]] + cor2[[13]] + plot_layout(ncol = 2)
  

ST0.1 <-rbindlist(ST0, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST0)


ST0.1 <- ST0 %>% select(1:3) %>% separate(1, c("loc", "year", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year), sep = "_", remove = T) 
head(ST0.1)
ST0.1 <-na.omit(ST0.1) 

ggplot(ST0.1, aes(x = env, y = predicted.value)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + facet_wrap(~trait, ncol = 4, scales = "free_y") + labs(y = "ST0 BLUP values", x = "") 


ST0.1 <- split(ST0.1, ST0.1$trait)
box0 <- list()
for (i in 1:length(ST0.1)) {
  a6 <- ST0.1[[i]] 
  lev3 <- names(ST0.1)[i]
  g1 <- ggplot(a6, aes(x = env, y = predicted.value)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + labs(title = paste(lev3, "ST0"), y = "", x = "")
  box0[[length(box0)+1]] <-  g1
}
names(box0) <- names(ST1)

box0[[7]] + box0[[6]] + box0[[1]] + box0[[3]] + plot_layout(ncol = 2)
box0[[4]] + box0[[9]] + box0[[11]] + box0[[15]] + plot_layout(ncol = 2)
box0[[5]] + box0[[14]] + box0[[10]] + box0[[12]] + plot_layout(ncol = 2)
box0[[8]] + box0[[16]] + box0[[2]] + box0[[13]] + plot_layout(ncol = 2)


