rm(list = ls())
session_info()

# quality
library(data.table)
library(tidyverse)
library(ggpubr)
library(StageWise)
library(asreml)
library(asremlPlus)
library(dplyr)

A1 <- read.csv("~/Documents/Cesar/git/quality_2022/N1.3.csv")
head(A1)
str(A1)
A2 <- A1 %>% dplyr::filter(!Year %in% c(2018, 2019))
colnames(A2)

lev2 <- colnames(A2[1:10])
A2[,lev2] <- lapply(A2[,lev2], factor)
str(A2)


A2 <- A2 %>% unite("merged", c(Location, Year, trait), sep = "_", remove = F)
A2 <- split(A2, A2$merged)
names(A2)
length(A2)

A3 <- A2[-112]
data <- A2[[112]]
names(A2)

qual_AIC <- list()
qual_BLUE <- list()
for (i in 1:length(A2)) {
  data <- A2[[i]]
  data <- data[order(data$row, data$col), ]
  
#  m1 <- asreml::asreml(fixed = resp ~ 1 + cov1 + cov2, 
                       # random = ~ + gen + block, residual = ~sar(row):sar(col), 
                       # data = data, 
                       # na.action = list(x = "include", y = "include"))
  
#  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       # random = ~ + block, residual = ~ar1(row):id(col), 
                       # data = data, 
                       # na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  # info1 <- infoCriteria.asreml(m1)
  # info2 <- infoCriteria.asreml(m2)
  # info3 <- infoCriteria.asreml(m3)
  # 
  # info1$model <- "sar_sar"
  # info2$model <- "ar1_id"
  # info3$model <- "ar1_ar1"
  # 
  # data1 <- rbind(info1, info3)
  # data1 <- rbind(info1, info2, info3)
  # qual_AIC[[length(qual_AIC)+1]] = data1
  # 
  # ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
  #        blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
  #        ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
  #               blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
  #               ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
  #                      blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  # 
  blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  qual_BLUE[[length(qual_BLUE)+1]] = blue
  
}

names(qual_BLUE) <- names(A2)
# names(qual_AIC) <- names(A2)

qual_AIC1 <- c(list('WA_2021_Fat'=data1), qual_AIC)
qual_BLUE1 <- c(list('WA_2021_Fat'=blue), qual_BLUE)

qual_AIC2 <-rbindlist(qual_AIC1, use.names=TRUE, fill=TRUE, idcol="trait")
qual_AIC2[ , .SD[which.min(AIC)], by = trait]
str(qual_AIC2)
cc <- count(qual_AIC2, model)

qual_BLUE2 <-rbindlist(qual_BLUE, use.names=TRUE, fill=TRUE, idcol="trait")
head(qual_BLUE2)
qual_BLUE4.1 <- qual_BLUE2 %>% separate(1, c("env", "year", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge")

list_5.1 <- c("env", "year", "gen", "trait")

class(qual_BLUE4.1)
qual_BLUE4.1 <- as.data.frame(qual_BLUE4.1)
qual_BLUE4.1[list_5.1] <- lapply(qual_BLUE4.1[list_5.1], factor)
str(qual_BLUE4.1)
colnames(qual_BLUE4.1)
qual_BLUE4.1 <- qual_BLUE4.1[,-7]
levels(qual_BLUE4.1$trait)

head(qual_BLUE4.1)
qual_BLUE5 <- split(qual_BLUE4.1, qual_BLUE4.1$trait)
class(qual_BLUE5[[1]])
names(qual_BLUE5)
# data <- qual_BLUE5[[1]]


qual_BLUP1 <- list()
qual_BLUP2 <- list()
qual_AIC3 <- list()

for (i in 1:(length(qual_BLUE5))) {
  data <- qual_BLUE5[[i]]
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  head(data1)
  
  Diag <- asreml::asreml(fixed = BLUE ~ 1 + year, 
                         random = ~ + diag(env):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  US <- asreml::asreml(fixed = BLUE ~ 1 + year,
                       random = ~ + idv(env):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  FA_1 <- asreml::asreml(fixed = BLUE ~ 1 + year, 
                         random = ~ + fa(env, 1):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  FA_1 <- update.asreml(FA_1)
  
  CORGH <- asreml::asreml(fixed = BLUE ~ 1 + year,
                          random = ~ + corgh(env):id(gen),
                          data = data1, na.action = list(x = "include", y = "include"),
                          weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  i1 <- infoCriteria.asreml(Diag)
  i2 <- infoCriteria.asreml(US)
  i3 <- infoCriteria.asreml(FA_1)
  i4 <- infoCriteria.asreml(CORGH)
  
  i1$model <- "Diag"
  i2$model <- "US"
  i3$model <- "FA_1"
  i4$model <- "CORGH"
  
  data2 <- rbind(i1, i2, i3, i4)
  qual_AIC3[[length(qual_AIC3)+1]] = data2
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC && i1$AIC < i4$AIC, 
         c(BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = Diag, 
                                wald.tab = NULL, 
                                present = c("env", "gen"))$predictions,
           BLUP4 <- predictPlus(classify = "gen", asreml.obj = Diag, 
                                wald.tab = NULL, 
                                present = c("env", "gen"))$predictions),
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC && i2$AIC < i4$AIC,
                c(BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env", "gen"))$predictions,
                  BLUP4 <- predictPlus(classify = "gen", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env", "gen"))$predictions),
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC && i3$AIC < i4$AIC, 
                       c(BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env", "gen"))$predictions,
                         BLUP4 <- predictPlus(classify = "gen", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env", "gen"))$predictions),
                       ifelse(i4$AIC < i1$AIC && i4$AIC < i2$AIC && i4$AIC < i3$AIC, 
                              c(BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = CORGH, 
                                                     wald.tab = NULL, 
                                                     present = c("env", "gen"))$predictions,
                                BLUP4 <- predictPlus(classify = "gen", asreml.obj = CORGH, 
                                                     wald.tab = NULL, 
                                                     present = c("env", "gen"))$predictions)
                       ))))
  
  BLUP3 <- BLUP3[,c(1:3)]
  BLUP4 <- BLUP4[,c(1:2)]
  BLUP3$env <- gsub("^", "ST1_", BLUP3$env)
  BLUP3$env <- paste(BLUP3$env, names(qual_BLUE5[i]), sep = "_", collapse = NULL)
  BLUP4$env <- paste("ST2", names(qual_BLUE5[i]), sep = "_", collapse = NULL)
  qual_BLUP1[[length(qual_BLUP1)+1]] <- BLUP3
  qual_BLUP2[[length(qual_BLUP2)+1]] <- BLUP4
  
}

