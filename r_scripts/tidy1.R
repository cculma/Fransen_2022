rm(list = ls())
session_info()

# quality
library(data.table)
library(tidyverse)
library(ggpubr)
library(StageWise)
library(asreml)
library(asremlPlus)
library(stringr)
library(PMCMRplus)

setwd("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/quality_2022")
data_qual <- list.files(pattern = ".csv", full.names = T)

# qual1 contain all entries 2018
# qual2 contain 201 & 202 2018
# qual3 contain 201 & 202 2019 ID
# qual4 contain 201 & 202 2019 OR
# qual5 contain 201 & 202 2019 WA

qual1 <- list()
for (i in 1:length(data_qual)) {
  data <- read.csv(data_qual[i])
  qual1[[length(qual1)+1]] <-  data
}

a1 <- read.csv("~/Documents/git/Norberg_2020/spatial_distribution/cols_rows1.csv")
head(a1)

A2018_1 <- qual1[[1]]
A2018_1 <- A2018_1[1:(length(A2018_1)-5)]
colnames(A2018_1)
A2018_1 <- A2018_1[,-c(3,4)]
A2018_1 <- left_join(a1, A2018_1, by = "ID")
A2018_1$Year <- "18"
A2018_1$Cutting <- 1
A2018_1 <- A2018_1 %>% gather(key = "trait", value = "raw", 9:75)


A2018_2 <- qual1[[2]]
A2018_2 <- A2018_2[,-c(3:6)]
colnames(A2018_2)
A2018_2 <- inner_join(a1, A2018_2, by = "ID")
A2018_2 <- A2018_2 %>% gather(key = "trait", value = "raw", 10:24)

# ID2019 <- qual1[[3]]
# ID2019 <- ID2019[,-c(2,3,5)]
# colnames(ID2019)
# ID2019.2 <- ID2019 %>% dplyr::filter(Cutting %in% c(2))
# ID2019.2 <- inner_join(a1, ID2019.2, by = "ID")

ID2019 <- qual1[[3]]
OR2019 <- qual1[[4]]
WA2019 <- qual1[[5]]
colnames(ID2019)
colnames(OR2019)
colnames(WA2019)

ID2019 <- ID2019[,-c(1,2,3,5)]
OR2019 <- OR2019[,-c(1,2,3,5)]
WA2019 <- WA2019[,-c(1,2,3,5,6)]

A2019 <- rbind(ID2019, OR2019, WA2019)
A2019$Cutting <- as.factor(A2019$Cutting)
levels(A2019$Cutting)
A2019 <- inner_join(a1, A2019, by = "ID")
colnames(A2019)
A2019$Year <- "19"
A2019 <- A2019 %>% gather(key = "trait", value = "raw", 9:26)

colnames(A2018_1)
head(A2018_1)
dim(A2018_1)

colnames(A2018_2)
colnames(A2019)
head(A2019)

qual2 <- rbind(A2018_2, A2019)
qual2 <- rbind(A2018_1, A2018_2, A2019)
lev1 <- colnames(qual2)
lev1 <- lev1[-11]
qual2[,lev1] <- lapply(qual2[,lev1], factor)
str(qual2)
colnames(qual2)[9] <- "Cut"
colnames(qual2)
summary(qual2)
levels(qual2$trait)
summary(qual2$trait)
summary(qual2$Treatment)

qual3 <- qual2 %>% unite("merged", c(Location, Year, Cut, trait), sep = "_", remove = F)
qual3 <- qual2 %>% unite("merged", c(Location, Year, Cut), sep = "_", remove = F)
qual3$merged <- as.factor(qual3$merged)
str(qual3)
qual4 <- split(qual3, qual3$merged)
length(qual4)

names(qual4)
cc1 <- count(qual4[["WA_19_4_Starc"]], Treatment)
cc2 <- count(qual4[["WA_19_4_aNDF"]], Treatment)
cc3 <- count(qual4[["OR_19_4_aNDF"]], Treatment)
cc4 <- count(qual4[["OR_19_4_Starc"]], Treatment)

levels(qual4[["ID_18_2"]]$trait)
levels(qual4[["ID_18_3"]]$trait)
cc5 <- count(qual4[["ID_18_2"]], Treatment, trait)
cc5 %>% spread(key = trait, value = n)

qual6 <- list()
for (i in 1:(length(qual4))) {
  cc5 <- count(qual4[[i]], Treatment, trait)
  cc5 <- cc5 %>% spread(key = trait, value = n)
  qual6[[length(qual6)+1]] <- cc5
}
names(qual6) <- names(qual4)

colnames(qual6[["ID_18_2"]])
colnames(qual6[["ID_19_3"]])


A2018_1[,lev1] <- lapply(A2018_1[,lev1], factor)
str(A2018_1)
colnames(A2018_1)[9] <- "Cut"
colnames(A2018_1)[5] <- "gen"
colnames(A2018_1)[11] <- "resp"
colnames(A2018_1)[2] <- "block"

colnames(A2018_1)
head(A2018_1)
# A2018_1 <- na.omit(A2018_1)
summary(A2018_1)
levels(A2018_1$trait)
summary(A2018_1$trait)
summary(A2018_1$gen)

c201 <- A2018_1 %>% dplyr::filter(gen %in% c(201))
c202 <- A2018_1 %>% dplyr::filter(gen %in% c(202))
c201 <- c201[,-c(3:7)]
colnames(c201)[6] <- "cov1"
c202 <- c202[,-c(3:7)]
colnames(c202)[6] <- "cov2"

a3 <- left_join(a3, c201, by= c("Location", "Year", "Cut", "block", "trait")) %>% left_join(., c202, by= c("Location", "Year", "Cut", "block",  "trait"))
head(b1.3)


qual5 <- b1.3 %>% unite("merged", c(Location, trait), sep = "_", remove = F)
qual5$merged <- as.factor(qual5$merged)
str(qual5)

data_subset <- qual5[,"cov2"]
qual5 <- qual5[complete.cases(data_subset),]

qual6 <- split(qual5, qual5$merged)
qual6[[145]]

qual7 <- qual6[-c(289,371)]
names(qual6)
head(qual6[[371]])
head(data)
data <- qual6[[289]]
data <- qual6[[371]]

# WA_Lig_NEG_18 [289]
# WA_T_Milk48_NEG_18 [371]

#######################
# ST0
qual_AIC <- list()
qual_BLUE <- list()
for (i in 1:length(qual7)) {
  data <- qual7[[i]]
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~  + block, residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)
  
  info1$model <- "sar_sar"
  info2$model <- "ar1_id"
  info3$model <- "ar1_ar1"
  
  # data1 <- rbind(info1, info2)
  data1 <- rbind(info1, info2, info3)
  qual_AIC[[length(qual_AIC)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
#  qual_BLUE[[length(qual_BLUE)+1]] = blue
  
}
summary(m1)$varcomp
vpredict(m1, h2 ~ V2/V2 + V3 + V4)

# wald.asreml(m2, ssType = "conditional", denDF = "numeric")


names(qual_BLUE) <- names(qual7)
names(qual_AIC) <- names(qual7)

qual_AIC1 <- c(list('WA_Lig_NEG_18'=data1),qual_AIC)
qual_BLUE1 <- c(list('WA_Lig_NEG_18'=blue), qual_BLUE)
qual_AIC1 <- c(list('WA_T_Milk48_NEG_18'=data1),qual_AIC1)
qual_BLUE1 <- c(list('WA_T_Milk48_NEG_18'=blue), qual_BLUE1)

qual_AIC2 <-rbindlist(qual_AIC1, use.names=TRUE, fill=TRUE, idcol="trait")
qual_AIC2[ , .SD[which.min(AIC)], by = trait]
cc <- count(qual_AIC2, model)

qual_BLUE2 <-rbindlist(qual_BLUE1, use.names=TRUE, fill=TRUE, idcol="trait")
head(qual_BLUE2)

qual_BLUE3 <- qual_BLUE2 %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(qual_BLUE3)[2:length(qual_BLUE3)] <- gsub("^", "ST0_", colnames(qual_BLUE3)[2:length(qual_BLUE3)])
colnames(qual_BLUE3)

#######
head(qual_BLUE4.1)
head(qual_BLUE4)

qual_BLUE4 <- qual_BLUE2 %>% separate(1, c("env", "trait1"), sep = "_", remove = F, convert = FALSE, extra = "merge")
qual_BLUE4$year <- str_sub(qual_BLUE4$trait,-2,-1)
qual_BLUE4$trait <- str_sub(qual_BLUE4$trait1, end=-4)
qual_BLUE4 <- qual_BLUE4[,-3]
qual_BLUE4 <- qual_BLUE4[,c(3,2,8,1,4,5,7)]


list_5.1 <- c("env", "year", "gen", "trait")
class(qual_BLUE4)
qual_BLUE4 <- as.data.frame(qual_BLUE4)
qual_BLUE4[list_5.1] <- lapply(qual_BLUE4[list_5.1], factor)
str(qual_BLUE4)
qual_BLUE4.2 <- qual_BLUE4

library(plyr)
qual_BLUE4.2$year <- revalue(qual_BLUE4.2$year, c("18" = "2018"))
qual_BLUE4.2$year <- revalue(qual_BLUE4.2$year, c("19" = "2019"))
str(qual_BLUE4.2)

levels(qual_BLUE4.2$trait)
levels(qual_BLUE4.1$trait)

hist(blue$predicted.value)
hist(A2[[1]]$resp)

###############
# ST1
list_5.1 <- c("gen","env","year","trait")

class(qual_BLUE4)
qual_BLUE4 <- as.data.frame(qual_BLUE4)
qual_BLUE4[list_5.1] <- lapply(qual_BLUE4[list_5.1], factor)
str(qual_BLUE4)  
head(qual_BLUE4)
qual_BLUE5 <- split(qual_BLUE4, qual_BLUE4$trait)
class(qual_BLUE5[[1]])
names(qual_BLUE5)
data <- qual_BLUE5[[1]]

# qual_BLUP1 <- list()
qual_BLUP2 <- list()
qual_BLUP3 <- list()
qual_BLUP4 <- list()
qual_AIC3 <- list()

for (i in 1:(length(qual_BLUE5))) {
  data <- qual_BLUE5[[i]]
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
  
  # FA_2 <- asreml::asreml(fixed = BLUE ~ 1 + env + year,
  #                        random = ~ + fa(env, 2):id(gen),
  #                        data = data1, na.action = list(x = "include", y = "include"),
  #                        weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  # FA_2 <- update.asreml(FA_2)

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
                         BLUP3 <- predictPlus(classify = "gen:year", asreml.obj = FA_1, 
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
BLUP4 <- BLUP4[,c(1:2)]
# head(BLUP2)
# BLUP2 <- BLUP2 %>% unite("env", c(env, year), sep = "_", remove = T)


BLUP2$env <- gsub("^", "ST1_", BLUP2$env)
BLUP2 <- split(BLUP2, with(BLUP2, env), drop = TRUE)

# BLUP3$env <- gsub("^", "ST2_", BLUP3$env)
# BLUP3$env <- paste(BLUP3$env, names(qual_BLUE5[i]), sep = "_", collapse = NULL)
# BLUP4$env <- paste("ST3", names(qual_BLUE5[i]), sep = "_", collapse = NULL)

qual_BLUP2[[length(qual_BLUP2)+1]] <- BLUP2
qual_BLUP3[[length(qual_BLUP3)+1]] <- BLUP3
qual_BLUP4[[length(qual_BLUP4)+1]] <- BLUP4

}

names(qual_BLUP2) <- names(qual_BLUE5)
names(qual_BLUP3) <- names(qual_BLUE5)
names(qual_BLUP4) <- names(qual_BLUE5)

plot(FA_1)


head(BLUP2)
str(BLUP2)
colnames(BLUP2)[4] <- "BLUE"
# BLUP2.1 <- BLUP2 %>% unite("env", c(env, year), sep = "_", remove = T)
# head(BLUP2.1)
# head(data)
BLUP2.1 <- split(BLUP2, with(BLUP2, env), drop = TRUE)
# BLUP2.1 <- split(data, with(data, env), drop = TRUE)
colnames(BLUP2.1[["ID"]])
D1 <- BLUP2
D1 <- BLUP2.1
a3 <- BLUP2.1[[3]]

# colnames(BLUP3)[3] <- "BLUE"
# D1 <- BLUP3

class(qual_BLUP2[["ADF"]][["ST1_ID"]])


class(qual_BLUP2[[1]][[2]])

Dun_201 <- list()
Dun_202 <- list()

for (i in 1:(length(qual_BLUP2))) {
  B1 <- qual_BLUP2[[i]]
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
    
    model_201 <- aov(predicted.value ~ gen, data = D4)
    model_202 <- aov(predicted.value ~ gen, data = D5)
    
    D6 <- summary(dunnettTest(model_201))
    D7 <- as.data.frame(D6[["p.value"]])
    colnames(D7) <- paste(colnames(D7), names(qual_BLUP2[i]), names(B1[j]), sep = "_", collapse = NULL)
    D8 <- summary(dunnettTest(model_202))
    D9 <- as.data.frame(D8[["p.value"]])
    colnames(D9) <- paste(colnames(D9), names(qual_BLUP2[i]), names(B1[j]), sep = "_", collapse = NULL)
    
    Dun_201[[length(Dun_201)+1]] <- D7
    Dun_202[[length(Dun_202)+1]] <- D9
  }
}



D1 <- qual_BLUP3[["ADF"]]

D201 <- D1 %>% dplyr::filter(gen %in% c(201))
D202 <- D1 %>% dplyr::filter(gen %in% c(202))
D3 <- D1 %>% dplyr::filter(!gen %in% c(201,202))
D3$gen <- gsub("^", "N_", D3$gen)
D201$gen <- gsub("^", "C_", D201$gen)
D202$gen <- gsub("^", "C_", D202$gen)
lev5 <- c("env","gen")
# lev5 <- c("gen")

D4 <- rbind(D201, D3)
D5 <- rbind(D202, D3)
D4[,lev5] <- lapply(D4[,lev5], factor)
D5[,lev5] <- lapply(D5[,lev5], factor)

D4$gen <- as.factor(D4$gen)
head(D4)
str(D4)
# D4 <- D4 %>% spread(key = gen, value = BLUE) %>% column_to_rownames(var = "env")
# D4 <- D4[,-c(9)]

model_201 <- aov(predicted.value ~ gen, data = D4)
model_202 <- aov(predicted.value ~ gen, data = D5)
# levels(D4$gen)
summary(model_201)
summary(model_202)
# two_way_201 <- aov(BLUE ~ gen + env, data = D4)
# summary(two_way_201)

D6 <- summary(dunnettTest(model_201))
D7 <- as.data.frame(D6[["p.value"]])
colnames(D7) <- paste(colnames(D7), names(qual_BLUP2[1]), names(B1[1]), sep = "_", collapse = NULL)


# , alternative = "greater"))
summary(dunnettTest(model_202, alternative = "less"))

summary(dunnettTest(model_201, alternative = "greater"))
summary(dunnettTest(model_202, alternative = "less"))


summary(FA_1)$varcomp
names(qual_BLUP1) <- names(qual_BLUE5)
names(qual_BLUP2) <- names(qual_BLUE5)
names(qual_AIC3) <- names(qual_BLUE5)


leveneTest(BLUE ~ gen, data = D4) 
# test normally distributed residuals
y <- as.matrix(D4)
plot(FA_1)
ols_test



qual_BLUP1.1 <-rbindlist(qual_BLUP1, use.names=TRUE, fill=TRUE, idcol="trait")
qual_BLUP2.1 <-rbindlist(qual_BLUP2, use.names=TRUE, fill=TRUE, idcol="trait")
qual_AIC3 <-rbindlist(qual_AIC3, use.names=TRUE, fill=TRUE, idcol="trait")

head(qual_BLUP1.1)
head(qual_BLUP2.1)

qual_BLUP1.1 <- qual_BLUP1.1 %>% dplyr::select(2:4) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

qual_BLUP2.1 <- qual_BLUP2.1 %>% dplyr::select(2:4) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

qual_BLUP3 <- qual_BLUE3 %>% inner_join(., qual_BLUP1.1, by = "gen") %>% inner_join(., qual_BLUP2.1, by = "gen")


US1 <- predictPlus(classify = "env:gen", asreml.obj = US, 
                     wald.tab = NULL, 
                     present = c("env", "gen"))
anova(US)
summary(US)$varcomp
plot()
