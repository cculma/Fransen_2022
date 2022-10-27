library(RColorBrewer)
library(data.table)
library(tidyverse)
library(ggpubr)
library(StageWise)
library(asreml)
library(asremlPlus)
library(stringr)
library(PMCMRplus)
library(patchwork)
library(goeveg)

names(ST1)

str(qual_BLUP2[["CP"]])

a6 <- ST1[[6]]
head(ST1[[6]])

ST1.1 <-rbindlist(ST1, use.names=TRUE, fill=TRUE, idcol="trait") %>% unite("env", c(trait, loc), sep = "_", remove = T) %>% spread(key = env, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
head(ST1.1)
ggplot(ST1.1, aes(x = env, y = predicted.value)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + facet_wrap(~trait, ncol = 4, scales = "free_y") + labs(y = "ST1 BLUE values", x = "") 


box1 <- list()
for (i in 1:length(ST1)) {
  a6 <- ST1[[i]] %>% separate(2, c("loc", "year"), sep = "_", remove = F, convert = FALSE, extra = "merge")
  lev3 <- names(ST1)[i]
  g1 <- ggplot(a6, aes(x = env, y = predicted.value)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + labs(title = paste(lev3, "ST1"), y = "", x = "")
  box1[[length(box1)+1]] <-  g1
}
names(box1) <- names(ST1)

box1[[7]] + box1[[6]] + box1[[1]] + box1[[3]] + plot_layout(ncol = 2)
box1[[4]] + box1[[9]] + box1[[11]] + box1[[15]] + plot_layout(ncol = 2)
box1[[5]] + box1[[14]] + box1[[10]] + box1[[12]] + plot_layout(ncol = 2)
box1[[8]] + box1[[16]] + box1[[2]] + box1[[13]] + plot_layout(ncol = 2)

cor1[[2]] + cor2[[6]] + cor3[[6]] + raw_CP + box0[[6]] + box1[[6]] + plot_layout(ncol = 3)



ST2.1 <-rbindlist(ST2, use.names=TRUE, fill=TRUE, idcol="trait") %>% unite("env", c(trait, loc), sep = "_", remove = T) %>% spread(key = env, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")

ST2.1 <-rbindlist(ST2, use.names=TRUE, fill=TRUE, idcol="trait") 
head(ST2.1)
ggplot(ST2.1, aes(x = loc, y = predicted.value)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + facet_wrap(~trait, ncol = 4, scales = "free_y") + labs(y = "ST2 BLUE values", x = "") 
# 800 800

head(ST2[[1]])
box2 <- list()
for (i in 1:length(ST2)) {
  a6 <- ST2[[i]]
  lev3 <- names(ST2)[i]
  g1 <- ggplot(a6, aes(x = loc, y = predicted.value)) + geom_boxplot(alpha = 0.6, outlier.shape = NA, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + labs(title = lev3, y = "", x = "") 
  
  box2[[length(box2)+1]] <-  g1
}
names(box2) <- names(ST2)
box2[[7]] + box2[[6]] + box2[[1]] + box2[[3]] + plot_layout(ncol = 2)
box2[[4]] + box2[[9]] + box2[[11]] + box2[[15]] + plot_layout(ncol = 2)
box2[[5]] + box2[[14]] + box2[[10]] + box2[[12]] + plot_layout(ncol = 2)
box2[[8]] + box2[[16]] + box2[[2]] + box2[[13]] + plot_layout(ncol = 2)

a7 <- list()
for (i in 1:(length(a6))) {
  data <- a6[[i]]
  data$norm <- range01(data$predicted.value)
  a7[[length(a7)+1]] <- data
}
names(a7) <- names(a6)

a7 <-rbindlist(a7, use.names=TRUE, fill=TRUE, idcol="trait")
head(a7)
a7 <- a7[,-1]

ggplot(a7, aes(x = env, y = norm)) + geom_boxplot(alpha = 0.6, outlier.shape = NA, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + scale_fill_brewer(palette = "Set1") + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12)) + labs(title = lev3, y = "", x = "") 



##########
# boxplot scaled ST1 BLUE by gen

names(SC1)

head(SC1[[1]])
box5 <- list()
for (i in 1:length(SC1)) {
  a6 <- SC1[[i]]
  a3.1 <- a6 %>% group_by(gen) %>% summarise_at(vars(scaled), list(mean = mean), na.rm = TRUE) %>% dplyr::filter(mean > 0.5 | mean < -0.5)
  lev2 <- a3.1$gen
  a3.2 <- a6 %>% dplyr::filter(gen %in% lev5) %>% mutate(type=ifelse(gen %in% c("201","202"),"Highlighted","Normal"))
  lev3 <- names(SC1)[i]
  g1 <- ggplot(a3.2, aes(x = reorder(gen, scaled, na.rm = TRUE), y = scaled, fill = type)) + geom_boxplot(alpha = 0.6, width=0.8, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial", base_size = 10) + scale_fill_brewer(palette = "Set1") + theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 8)) + labs(title = lev3, y = "", x = "") 
  box5[[length(box5)+1]] <-  g1
}

lev4 <- sample.int(200, 50)
lev5 <- c(lev4, 201, 202)

names(box5) <- names(SC1)
box5[[7]] + box5[[6]] + box5[[1]] + box5[[3]] + plot_layout(ncol = 2)
box5[[4]] + box5[[9]] + box5[[11]] + box5[[15]] + plot_layout(ncol = 2)
box5[[5]] + box5[[14]] + box5[[10]] + box5[[12]] + plot_layout(ncol = 2)
box5[[8]] + box5[[16]] + box5[[2]] + box5[[13]] + plot_layout(ncol = 2)
# 1200 500

###############

# boxplot scaled ST2 BLUE by gen

names(SC1)

head(SC1[[1]])
box6 <- list()
for (i in 1:length(SC1)) {
  a6 <- SC1[[i]]
  a3.1 <- a6 %>% group_by(gen) %>% summarise_at(vars(scaled), list(mean = mean), na.rm = TRUE) %>% dplyr::filter(mean > 0.5 | mean < -0.5)
  lev2 <- a3.1$gen
  a3.2 <- a6 %>% dplyr::filter(gen %in% lev5) %>% mutate(type=ifelse(gen %in% c("201","202"),"Highlighted","Normal"))
  lev3 <- names(SC1)[i]
  g1 <- ggplot(a3.2, aes(x = reorder(gen, scaled, na.rm = TRUE), y = scaled, fill = type)) + geom_boxplot(alpha = 0.6, width=0.8, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial", base_size = 10) + scale_fill_brewer(palette = "Set1") + theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 8)) + labs(title = lev3, y = "", x = "") 
  box6[[length(box6)+1]] <-  g1
}

lev4 <- sample.int(200, 50)
lev5 <- c(lev4, 201, 202)

names(box6) <- names(SC1)
box6[[7]] + box6[[6]] + box6[[1]] + box6[[3]] + plot_layout(ncol = 2)
box6[[4]] + box6[[9]] + box6[[11]] + box6[[15]] + plot_layout(ncol = 2)
box6[[5]] + box6[[14]] + box6[[10]] + box6[[12]] + plot_layout(ncol = 2)
box6[[8]] + box6[[16]] + box6[[2]] + box6[[13]] + plot_layout(ncol = 2)
# 1200 500


#################
names(SC1)
a6 <- SC1[[6]]
a3.1 <- a6 %>% group_by(gen) %>% summarise_at(vars(scaled), list(mean = mean), na.rm = TRUE)
a3.1 <- a6 %>% group_by(gen) %>% summarise_at(vars(scaled), list(min = min, max = max, mean = mean, sd = sd, cv = cv, var = var), na.rm = TRUE)



x_mid <- 1
y_mid <- 0

ggplot(a3.1, aes(x=sd, y=mean, label = gen)) + geom_point() + geom_text(label=a3.1$gen) + geom_vline(xintercept = x_mid) + geom_hline(yintercept = y_mid) 
