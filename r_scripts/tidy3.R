library(data.table)
library(StageWise)
library(asreml)
library(asremlPlus)
library(stringr)
library(PMCMRplus)
library(plyr)
library(ggcorrplot)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(variability)

save.image("~/Documents/Cesar/git/quality_2022/tidy2.RData")
load("~/Documents/Cesar/git/quality_2022/tidy2.RData")
levels(qual_BLUE4.2$trait) # 2018, 2019
levels(qual_BLUE4.1$trait) # 2020, 2021


lev3 <- c("ADF","ADICP","aNDF","Ash","Ca","CP","DM","ESC","Fat","K","Lignin","Mg","NDICP","P","Starch","WSC")


# qual_BLUE4.2$trait <- revalue(qual_BLUE4.2$trait, c("Cprot" = "CP"))
# qual_BLUE4.2$trait <- revalue(qual_BLUE4.2$trait, c("Ligin" = "Lignin"))
# qual_BLUE4.2$trait <- revalue(qual_BLUE4.2$trait, c("Starc" = "Starch"))
# qual_BLUE4.2$trait <- revalue(qual_BLUE4.2$trait, c("Su_WS" = "WSC"))
# qual_BLUE4.2$trait <- revalue(qual_BLUE4.2$trait, c("Su_ES" = "ESC"))

qual_BLUE4.1 <- qual_BLUE4.1 %>% dplyr::filter(trait %in% lev3)
qual_BLUE4.2 <- qual_BLUE4.2 %>% dplyr::filter(trait %in% lev3)

str(qual_BLUE4.1)
str(qual_BLUE4.2)
qual_BLUE4.2$trait <- droplevels(qual_BLUE4.2$trait)
qual_BLUE4.2$year <- droplevels(qual_BLUE4.2$year)

summary(qual_BLUE4.1$trait)
summary(qual_BLUE4.2$trait)

qual_BLUE4.3 <- rbind(qual_BLUE4.1, qual_BLUE4.2) 
summary(qual_BLUE4.3$trait)
str(qual_BLUE4.3)
head(qual_BLUE4.3)
qual_BLUE4.3$trait <- droplevels(qual_BLUE4.3$trait)
qual_BLUE4.3$year <- droplevels(qual_BLUE4.3$year)


# qual_BLUE4.4 <- qual_BLUE4.3 %>% unite("merged", c(env, trait), sep = "_", remove = F)
# qual_BLUE4.4 <- split(qual_BLUE4.4, qual_BLUE4.4$merged)
# names(qual_BLUE4.4)

qual_BLUE4.4 <- split(qual_BLUE4.1, qual_BLUE4.1$trait)  # 2020, 2021
qual_BLUE4.4 <- split(qual_BLUE4.2, qual_BLUE4.2$trait)
qual_BLUE4.4 <- split(qual_BLUE4.3, qual_BLUE4.3$trait)
names(qual_BLUE4.4)




# qual_BLUP1 <- list()
qual_BLUP2 <- list()
qual_BLUP3 <- list()
qual_BLUP4 <- list()
# qual_AIC3 <- list()

names(qual_BLUE4.4)
data <- qual_BLUE4.4[[6]]
# head(data)
# colnames(data)[2] <- "loc"
# data <- data %>% unite("env", c(loc, year), sep = "_", remove = F) 
# data$env <- as.factor(data$env)
# data$year <- factor(data$year, levels=c('2018', '2019', '2020', '2021'))
str(data)
# levels(data$year)
# levels(data$env)

for (i in 1:(length(qual_BLUE4.4))) {
  data <- qual_BLUE4.4[[i]]
  colnames(data)[2] <- "loc"
  data <- data %>% unite("env", c(loc, year), sep = "_", remove = F) 
  data$env <- as.factor(data$env)
  data$year <- factor(data$year, levels=c('2018', '2019', '2020', '2021'))
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  head(data1)
  FA_1 <- asreml::asreml(fixed = BLUE ~ 1 + loc, 
                         random = ~ fa(env, 1):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  FA_1 <- update.asreml(FA_1)
  
  BLUP2 <- predictPlus(classify = "env:gen", asreml.obj = FA_1,
                       wald.tab = NULL, present = c("gen","env","loc"))$predictions
  BLUP3 <- predictPlus(classify = "loc:gen", asreml.obj = FA_1, 
                       wald.tab = NULL, present = c("gen","env","loc"))$predictions
  BLUP4 <- predictPlus(classify = "gen", asreml.obj = FA_1, 
                       wald.tab = NULL, present = c("gen","env","loc"))$predictions
  
  BLUP2 <- BLUP2[,c(1:3)]
  BLUP3 <- BLUP3[,c(1:3)]
  BLUP4 <- BLUP4[,c(1:2)]
  
  qual_BLUP2[[length(qual_BLUP2)+1]] <- BLUP2
  qual_BLUP3[[length(qual_BLUP3)+1]] <- BLUP3
  qual_BLUP4[[length(qual_BLUP4)+1]] <- BLUP4
}  

names(qual_BLUP4) <- names(qual_BLUE4.4)
qual_BLUP4 <-rbindlist(qual_BLUP4, use.names=TRUE, fill=TRUE, idcol="loc")
head(qual_BLUP4)
qual_BLUP4 <- qual_BLUP4 %>% spread(key = loc, value = predicted.value)
write.csv(qual_BLUP4, "~/Documents/Cesar/git/quality_2022/BLUP_all_ST4.csv", row.names = F)
qual_BLUP4 <- qual_BLUP4 %>% remove_rownames() %>% column_to_rownames("gen")
BLUP2.1 <- cor(qual_BLUP4, use = "complete")

ggcorrplot(BLUP2.1, hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + scale_fill_gradient(low = "white", high = "orangered") + theme_classic(base_family = "Arial", base_size = 8) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())

ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "Pearson ST3")

var_com_CP <- summary(FA_1)$varcomp
infoCriteria.asreml(FA_1)
current.asrt <- as.asrtests(FA_1)
print(current.asrt, which = "pseudoanova")

names(qual_BLUP2)
qual_BLUP2 <-rbindlist(qual_BLUP2, use.names=TRUE, fill=TRUE, idcol="trait")
qual_BLUP2 <- split(qual_BLUP2, qual_BLUP2$trait)

BLUP2 <- qual_BLUP2[[1]]
head(BLUP2)
BLUP2 <- BLUP2 %>% select(-1)
BLUP2 <- BLUP2 %>% select(-1) %>% spread(key = env, value = predicted.value) %>% remove_rownames() %>% column_to_rownames("gen")

# BLUP2 <- BLUP2 %>% unite("env", c(loc, year), sep = "_", remove = T) %>% spread(key = env, value = predicted.value) %>% remove_rownames() %>% column_to_rownames("gen")

BLUP2.1 <- cor(BLUP2, use = "complete")

ggcorrplot(BLUP2.1[,ncol(BLUP2.1):1], hc.order = F, type = "full", lab = T, lab_col = "grey3", lab_size = 2, show.diag = T) + theme_classic(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank()) + labs(title = "CP")
# 600 600

ggcorrplot(BLUP2.1, type = "upper") + theme_classic(base_family = "Arial", base_size = 10) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank())


qual_BLUP3 <-rbindlist(qual_BLUP3, use.names=TRUE, fill=TRUE, idcol="trait")
head(qual_BLUP3)
BLUP3 <- qual_BLUP3 %>% unite("merged", c(trait, loc), sep = "_", remove = T) %>% spread(key = merged, value = predicted.value)
write.csv(BLUP3, "~/Documents/Cesar/git/quality_2022/BLUP_all_ST3.csv", row.names = F)
head(BLUP3)
BLUP3 <- BLUP3 %>% remove_rownames() %>% column_to_rownames("gen")
lev6 <- colnames(BLUP3)
lev7 <- lev6[c(1:9,16:18,22:27,31:33,40:42,46:48)]
BLUP3 <- BLUP3[,lev7]

BLUP2.1 <- cor(BLUP3, use = "complete")




for (i in 1:(length(qual_BLUE4.4))) {
  data <- qual_BLUE4.4[[i]]
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  head(data1)
  
  Diag <- asreml::asreml(fixed = BLUE ~ 1 + env + year, 
                         random = ~ + diag(env):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  US <- asreml::asreml(fixed = BLUE ~ 1 + env + year,
                       random = ~ + idv(env):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  FA_1 <- asreml::asreml(fixed = BLUE ~ 1 + env + year, 
                         random = ~ + fa(env, 1):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  FA_1 <- update.asreml(FA_1)
  
  CORGH <- asreml::asreml(fixed = BLUE ~ 1 + env + year,
                          random = ~ + corgh(env):id(gen),
                          data = data1, na.action = list(x = "include", y = "include"),
                          weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  i1 <- infoCriteria.asreml(Diag)
  i2 <- infoCriteria.asreml(US)
  i3 <- infoCriteria.asreml(FA_1)
  i4 <- infoCriteria.asreml(CORGH)
  # i5 <- infoCriteria.asreml(FA_2)
  
  i1$model <- "Diag"
  i2$model <- "US"
  i3$model <- "FA_1"
  i4$model <- "CORGH"
  
  
  data2 <- rbind(i1, i2, i3, i4)
  qual_AIC3[[length(qual_AIC3)+1]] = data2
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC && i1$AIC < i4$AIC, 
         c(BLUP2 <- predictPlus(classify = "env:gen:year", asreml.obj = Diag, 
                                wald.tab = NULL, 
                                present = c("env","gen","year"))$predictions,
           BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = Diag, 
                                wald.tab = NULL, 
                                present = c("env", "gen"))$predictions,
           BLUP4 <- predictPlus(classify = "gen", asreml.obj = Diag, 
                                wald.tab = NULL, 
                                present = c("env", "gen"))$predictions),
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC && i2$AIC < i4$AIC,
                c(BLUP2 <- predictPlus(classify = "env:gen:year", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env","gen","year"))$predictions,
                  BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env", "gen"))$predictions,
                  BLUP4 <- predictPlus(classify = "gen", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env", "gen"))$predictions),
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC && i3$AIC < i4$AIC, 
                       c(BLUP2 <- predictPlus(classify = "env:gen:year", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env","gen","year"))$predictions,
                         BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env","gen","year"))$predictions,
                         BLUP4 <- predictPlus(classify = "gen", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env","gen","year"))$predictions),
                       ifelse(i4$AIC < i1$AIC && i4$AIC < i2$AIC && i4$AIC < i3$AIC, 
                              c(BLUP2 <- predictPlus(classify = "env:gen:year", asreml.obj = CORGH, 
                                                     wald.tab = NULL, 
                                                     present = c("env","gen","year"))$predictions,
                                BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = CORGH, 
                                                     wald.tab = NULL, 
                                                     present = c("env", "gen"))$predictions,
                                BLUP4 <- predictPlus(classify = "gen", asreml.obj = CORGH, 
                                                     wald.tab = NULL, 
                                                     present = c("env", "gen"))$predictions)
                       ))))
  
  BLUP2 <- BLUP2[,c(1:4)]   
  BLUP3 <- BLUP3[,c(1:3)]
#  BLUP4 <- BLUP4[,c(1:2)]
  # head(BLUP2)
  # BLUP2 <- BLUP2 %>% unite("env", c(env, year), sep = "_", remove = T)
  
  
#  BLUP2$env <- gsub("^", "ST1_", BLUP2$env)
  BLUP2 <- split(BLUP2, with(BLUP2, env), drop = TRUE)
  
  # BLUP3$env <- gsub("^", "ST2_", BLUP3$env)
  # BLUP3$env <- paste(BLUP3$env, names(qual_BLUE5[i]), sep = "_", collapse = NULL)
  # BLUP4$env <- paste("ST3", names(qual_BLUE5[i]), sep = "_", collapse = NULL)
  
  qual_BLUP2[[length(qual_BLUP2)+1]] <- BLUP2
  qual_BLUP3[[length(qual_BLUP3)+1]] <- BLUP3
#  qual_BLUP4[[length(qual_BLUP4)+1]] <- BLUP4
  
}

names(qual_BLUP2) <- names(qual_BLUE4.4)
names(qual_BLUP3) <- names(qual_BLUE4.4)
# names(qual_BLUP4) <- names(qual_BLUE4.4)

head(qual_BLUP2[[1]])
a3 <- qual_BLUP3[["ADF"]]
a3 <- qual_BLUP2[["ADF"]][["WA"]]

a4 <- a3 %>% dplyr::filter(gen %in% c(201,202))
a5 <- a3 %>% dplyr::filter(!gen %in% c(201,202))
a4$entry <- "check"
a5$entry <- "new"
a3 <- rbind(a4, a5)
var(a3$predicted.value)


ggplot(a3, aes(x=reorder(gen, predicted.value), y = predicted.value, fill = entry)) + geom_boxplot(alpha = 0.6, outlier.shape = NA) + theme_classic(base_family = "Arial", base_size = 8) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(y = "ADF 2018-2021", x = "Genotype") + scale_fill_manual(values = c("new" = "azure", "check" = "blue2"))
# 1400 600

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

model_201 <- aov(predicted.value ~ gen, data = D4)
model_202 <- aov(predicted.value ~ gen, data = D5)
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

library(DescTools)
Dun_201 <- DunnettTest(x=D4$predicted.value, g=D4$gen)
DunnettTest(x=D5$predicted.value, g=D5$gen)
class(Dun_201[[1]])
Dun_201 <- as.data.frame(Dun_201[[1]])


names(qual_BLUP3)
Dun_201.1 <- list()
Dun_202.1 <- list()
# D1 <- qual_BLUP3[[1]]
for (i in 1:(length(qual_BLUP3))) {
  D1 <- as.data.frame(qual_BLUP3[[i]])
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
  
  D7 <- DunnettTest(x=D4$predicted.value, g=D4$gen)
  D9 <- DunnettTest(x=D5$predicted.value, g=D5$gen)
  D7 <- as.data.frame(D7[[1]])
  D9 <- as.data.frame(D9[[1]])
  
  Dun_201.1[[length(Dun_201.1)+1]] <- D7
  Dun_202.1[[length(Dun_202.1)+1]] <- D9
}


names(Dun_201.1) <- names(qual_BLUP3)
names(Dun_202.1) <- names(qual_BLUP3)

Dun_201.2 <- list()
Dun_202.2 <- list()

qual_BLUP2.1 <- qual_BLUP2[c(1,2)]


names(qual_BLUP2)

for (i in 1:(length(qual_BLUP2))) {
  B1 <- qual_BLUP2[[i]]
  D1.0 <- list()
  D2.0 <- list()
  for (j in 1:(length(B1))) {
    D1 <- as.data.frame(B1[[j]])
    
    D201 <- D1 %>% dplyr::filter(gen %in% c(201))
    D202 <- D1 %>% dplyr::filter(gen %in% c(202))
    D3 <- D1 %>% dplyr::filter(!gen %in% c(201,202))
    D3$gen <- gsub("^", "N_", D3$gen)
    D201$gen <- gsub("^", "C_", D201$gen)
    D202$gen <- gsub("^", "C_", D202$gen)
    
    D4 <- rbind(D201, D3)
    D5 <- rbind(D202, D3)
    D4[,lev5] <- lapply(D4[,lev5], factor)
    D5[,lev5] <- lapply(D5[,lev5], factor)
    
    D7 <- DunnettTest(x=D4$predicted.value, g=D4$gen)
    D9 <- DunnettTest(x=D5$predicted.value, g=D5$gen)
    D7 <- as.data.frame(D7[[1]])
    D9 <- as.data.frame(D9[[1]])
    D7$loc <- names(qual_BLUP2[[i]][j])
    D9$loc <- names(qual_BLUP2[[i]][j])

    D1.0[[length(D1.0)+1]] <- D7
    D2.0[[length(D2.0)+1]] <- D9
    Dun_201.2[[length(Dun_201.2)+1]] <- D1.0
    Dun_202.2[[length(Dun_202.2)+1]] <- D2.0
  }
}

names(Dun_202.2)

list1 <- seq(3, length(3:51),3)

Dun_201.3 <- Dun_201.2[list1]
Dun_202.3 <- Dun_202.2[list1]

names(Dun_201.3) <- names(qual_BLUP2)
names(Dun_202.3) <- names(qual_BLUP2)

Dun_201.4 <- list()
for (i in 1:(length(Dun_201.3))) {
  B1 <- Dun_201.3[[1]]
  B2 <- rbind(B1[[1]], B1[[2]], B1[[3]])
  B2 <- B2 %>% rownames_to_column(var = "comp")
  Dun_201.4[[length(Dun_201.4)+1]] <- B2
}  
Dun_202.4 <- list()
for (i in 1:(length(Dun_202.3))) {
  B1 <- Dun_202.3[[1]]
  B2 <- rbind(B1[[1]], B1[[2]], B1[[3]])
  B2 <- B2 %>% rownames_to_column(var = "comp")
  Dun_202.4[[length(Dun_202.4)+1]] <- B2
}  

names(Dun_201.4) <- names(qual_BLUP2)
names(Dun_202.4) <- names(qual_BLUP2)



qual_BLUP2.1 <- list()
for (i in 1:(length(qual_BLUP2))) {
  B1 <-rbindlist(qual_BLUP2[[i]], use.names=TRUE, fill=TRUE, idcol="loc")
  qual_BLUP2.1[[length(qual_BLUP2.1)+1]] <- B1
}
names(qual_BLUP2.1) <- names(qual_BLUP2)
qual_BLUP2.1 <-rbindlist(qual_BLUP2.1, use.names=TRUE, fill=TRUE, idcol="trait")
head(qual_BLUP2.1)
qual_BLUP2.1 <- qual_BLUP2.1[,-3]
qual_BLUP2.1 <- qual_BLUP2.1 %>% unite("merged", c(trait, loc, year), sep = "_", remove = T)
qual_BLUP2.1 <- qual_BLUP2.1 %>% spread(merged, predicted.value)
write.csv(qual_BLUP2.1, "~/Documents/Cesar/git/quality_2022/BLUP_RR_ST1.csv", row.names = F)


qual_BLUP3.1 <-rbindlist(qual_BLUP3, use.names=TRUE, fill=TRUE, idcol="trait")
head(qual_BLUP3.1)
qual_BLUP3.1 <- qual_BLUP3.1 %>% unite("merged", c(trait, env), sep = "_", remove = T)
qual_BLUP3.1 <- qual_BLUP3.1 %>% spread(merged, predicted.value)
write.csv(qual_BLUP3.1, "~/Documents/Cesar/git/quality_2022/BLUP_RR_ST2.csv", row.names = F)



Dun_201.4 <- rbindlist(Dun_201.4, use.names=TRUE, fill=TRUE, idcol="trait")
Dun_202.4 <- rbindlist(Dun_202.4, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(Dun_201.4, "~/Documents/Cesar/git/quality_2022/Dun_201.4.csv", row.names = F)
write.csv(Dun_202.4, "~/Documents/Cesar/git/quality_2022/Dun_202.4.csv", row.names = F)


Dun_201.5 <- rbindlist(Dun_201.1, use.names=TRUE, fill=TRUE, idcol="trait")
Dun_202.5 <- rbindlist(Dun_202.1, use.names=TRUE, fill=TRUE, idcol="trait")
