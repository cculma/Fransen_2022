head(ST0)

dat2 <- ST0 %>% separate(1, c("loc", "year", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year), sep = "_", remove = F) 
dat2 <- as.data.frame(dat2)
head(dat2)
dat2$env <- as.factor(dat2$env)
dat2$loc <- as.factor(dat2$loc)
dat2$year <- factor(dat2$year, levels=c('2018', '2019', '2020', '2021'))
dat2$trait <- as.factor(dat2$trait)
levels(dat2$year)
dat2$lab <- recode_factor(dat2$year, "2018" = "RR", "2019" = "RR", "2020" = "NI", "2021" = "NI",)
str(dat2)
class(dat2)
dat2 <- split(dat2, dat2$trait)
lev7 <- names(dat2)
# head(dat2[[6]])
# data <- dat2[[4]]
# head(data)
# hist(data$weight)

ST1 <- list()
ST2 <- list()
ST3 <- list()
SW_V <- list()
data <- dat2[[6]]
for (i in 1:(length(dat2))) {
  data <- dat2[[i]]
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  head(data1)
  # FA_1 <- asreml::asreml(fixed = predicted.value ~ 1,
  #                        random = ~ fa(env, 1):id(gen),
  #                        data = data1, na.action = list(x = "include", y = "include"),
  #                        family = asreml::asr_gaussian(dispersion = 1))


  FA_1 <- asreml::asreml(fixed = predicted.value ~ 1 + gen,
                         random = ~ fa(env, 1):id(gen) + loc,
                         data = data1, na.action = list(x = "include", y = "include"),
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  FA_1 <- update.asreml(FA_1)
  comp1 <- summary(FA_1)$varcomp
  SW_V[[length(SW_V)+1]] <- comp1

  # BLUP1 <- predict.asreml(FA_1, classify='gen:env', vcov=TRUE, )$pvals
  # BLUP2 <- predict.asreml(FA_1, classify='gen:loc', vcov=TRUE, )$pvals
  # BLUP3 <- predict.asreml(FA_1, classify='gen', vcov=TRUE, )$pvals
  # 
  # BLUP1 <- BLUP1[,c(1:3)]
  # BLUP2 <- BLUP2[,c(1:3)]
  # BLUP3 <- BLUP3[,c(1:2)]
  # 
  # ST1[[length(ST1)+1]] <- BLUP1
  # ST2[[length(ST2)+1]] <- BLUP2
  # ST3[[length(ST3)+1]] <- BLUP3
}  

names(ST1) <- names(dat2)
names(ST2) <- names(dat2)
names(ST3) <- names(dat2)
names(SW_V) <- names(dat2)

hist(BLUP1$predicted.value)
hist(BLUP1$std.error)

ggplot(BLUP1, aes(x = env, y = predicted.value)) + geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_bw(base_family = "Arial") + theme(legend.position = "none", panel.spacing = unit(0.3, "lines"), strip.text.x = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title = element_text(size = 12))

cor3 <- list() 
for (i in 1:length(ST1)) {
  a6 <- ST1[[i]] %>% spread(key = env, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
  BLUP2.1 <- cor(a6, use = "complete")
  lev3 <- names(ST1)[i]
  g1 <- ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(legend.position = "none" ,axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = paste(lev3, "ST1"))
  cor3[[length(cor3)+1]] <-  g1
}
names(cor3) <- names(ST1)

cor3[[7]] + cor3[[6]] + cor3[[1]] + cor3[[3]] + plot_layout(ncol = 2)
cor3[[4]] + cor3[[9]] + cor3[[11]] + cor3[[15]] + plot_layout(ncol = 2)
cor3[[5]] + cor3[[14]] + cor3[[10]] + cor3[[12]] + plot_layout(ncol = 2)
cor3[[8]] + cor3[[16]] + cor3[[2]] + cor3[[13]] + plot_layout(ncol = 2)

ST3.1 <-rbindlist(ST3, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST3.1)
ST3.1 <- ST3.1 %>% spread(key = trait, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
ST3.2 <- ST3.1 %>% dplyr::filter(trait %in% c("ADF","aNDF","Lignin")) %>% spread(key = trait, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
ST3.3 <- ST3.1 %>% dplyr::filter(trait %in% c("CP","Mg","P")) %>% spread(key = trait, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
ST3.4 <- ST3.1 %>% dplyr::filter(trait %in% c("Starch","ESC","WSC")) %>% spread(key = trait, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
ST3.5 <- ST3.1 %>% dplyr::filter(trait %in% c("Ca","K","Ash","DM")) %>% spread(key = trait, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")


BLUP2.1 <- cor(ST3.2, use = "complete")
install.packages('modelsummary')
setwd("~/Documents/Cesar/git/quality_2022/Pheno/")
library(modelsummary)
?datasummary_correlation
datasummary_correlation(ST3.5,
                        output = "ST3.5.docx")

datasummary_correlation(ST3.4,
                        output = "ST3.4.docx")

datasummary_correlation(ST3.3,
                        output = "ST3.3.docx")

datasummary_correlation(ST3.2,
                        output = "ST3.2.docx")

ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 3, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(legend.position = "none" ,axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "ST3")

ST2.1 <-rbindlist(ST2, use.names=TRUE, fill=TRUE, idcol="trait") %>% unite("env", c(trait, loc), sep = "_", remove = T) %>% spread(key = env, value = 3) %>% remove_rownames() %>% column_to_rownames("gen")
head(ST2.1)
BLUP2.1 <- cor(ST2.1, use = "complete")

ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = F, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "ST2")

ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "Pearson ST3")
