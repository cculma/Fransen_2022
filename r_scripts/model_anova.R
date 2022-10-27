
data <- qual6[[189]]
data <- data[order(data$row, data$col), ]
head(data)
str(data)
levels(data$row)
count(data, row)
levels(data$col)

H2 <- list()
for (i in 1:length(qual6)) {
  data <- qual6[[1]]
  data <- data[order(data$row, data$col), ]
  head(data)
  m1 <- asreml::asreml(fixed = resp ~ 1 + cov1 + cov2 + gen, 
                     random = ~  + block, residual = ~sar(row):sar(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))
  # h2 <- vpredict(m1, h2.resp ~ V2 /(V2 + V4 + V5))
  # h2$V_a <- m1$vparameters[2]
  h2 <- t(m1$vparameters)
  # h2 <- vpredict(m1, h2.resp ~ V1 /(V1 + V3 + V4))
  # h2$V_a <- m1$vparameters[1]
  
  H2[[length(H2)+1]] <- h2
}
# names(H2) <- names(qual6)
# H2 <-rbindlist(H2, use.names=TRUE, fill=TRUE, idcol="trait")
# class(h2)
library(plyr)
H2 <- rbind.fill.matrix(H2)
rownames(H2) <- names(qual6)
H2 <- as.data.frame(H2)
class(H2)
head(H2)
H2$h2 <- abs(H2$gen) / (abs(H2$gen) + abs(H2$block)+ abs(H2$`row:col!row!cor`) + abs(H2$`row:col!col!cor`))

H2$h2 <- abs(H2$gen) / (abs(H2$gen) + abs(H2$`row:col!row!cor`) + abs(H2$`row:col!col!cor`))
write.csv(H2, "~/Documents/Cesar/git/quality_2022/H2.csv")

m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                     random = ~ + block, residual = ~ar1(row):id(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m3 <- asreml::asreml(fixed = resp ~ 1 + cov1 + cov2, 
                     random = ~ + gen + block, residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m4 <- asreml::asreml(fixed = resp ~ 1 + block + cov1 + cov2, 
                     random = ~ + gen, residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

plot(m3)
plot(m4)
S1 <- as.data.frame(summary(FA_1)$varcomp)
write.csv(S1, "~/Documents/Cesar/git/quality_2022/S1.csv")
h2 <- vpredict(m1, h2.resp ~ V2 /(V2 + V4 + V5))
h2 <- vpredict(m1, h2.resp ~ V1 /(V1 + V3 + V4))
h2$V_a <- m1$vparameters[2]


vpredict(FA_1, h2.resp ~ V1 /(V1 + V3 + V4))
FA_1$vparameters
m1$vparameters
m1$vparameters[2]

print(m1, which = "pseudoanova")
as.asrtests(m1, NULL, NULL, label = "Maximal model", IClikelihood = "full")


head(FA_1$linear.predictors)
S2 <- as.data.frame(summary(m2, coef = T)$coef.fixed)
wald.asreml(FA_1, ssType = "incremental", denDF = "none") # default
wald.asreml(m2, ssType = "conditional", denDF = "numeric")
wald.asreml(m2, ssType = "conditional", denDF = "numeric")
wald.asreml(m2, ssType = "conditional", denDF = "numeric")
wald.asreml(m2, ssType = "conditional", denDF = "numeric")


wald.asreml(m1, ssType = "conditional", denDF = "numeric")
wald.asreml(m1, ssType = "incremental", denDF = "none")

?wald.asreml
m5 <- as.asrtests(m1, NULL, NULL)
# m5 <- rmboundary(m1, IClikelihood = "full")

summary(m1)$varcomp[1,3]/sum((m1)$varcomp[,3])

m5 <- rmboundary.asrtests(m1, IClikelihood = "full")
print(m1, which = "testsummary")
rm(m5)

BLUP4 <- predictPlus(classify = "gen", 
            asreml.obj = m1,
            sortFactor = "gen",
            wald.tab = m1$wald.tab)


for (i in 1:(length(qual_BLUE5))) {
  data <- qual_BLUE5[[1]]
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  head(data1)
  FA_1 <- asreml::asreml(fixed = BLUE ~ 1 + env + gen, 
                         random = ~ + fa(env, 1):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  FA_1 <- update.asreml(FA_1)
  
  Diag <- asreml::asreml(fixed = BLUE ~ 1 + env, 
                         random = ~ + diag(env):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
}

wald <- wald.asreml(FA_1, ssType = "incremental", denDF = "none")
class(wald)
write.csv(wald, "~/Documents/Cesar/git/quality_2022/wald.csv")


wald.asreml(FA_1, ssType = "incremental", denDF = "none")
summary(Diag)$varcomp
summary(FA_1)$varcomp

wald.asreml(Diag)

head(H2)
H2 <- H2 %>% rownames_to_column("trait")
dim(H2)

library(ggstatsplot)
Plot1 <- ggscatterstats(
  data = H2, ## dataframe from which variables are taken
  x = gen, ## predictor/independent variable
  y = h2, ## dependent variable
  xlab = "Genetic variance", ## label for the x-axis
  ylab = "Broad sense H2", ## label for the y-axis
  label.var = trait, ## variable to use for labeling data points
  label.expression = gen > 1, ## expression for deciding which points to label
  point.label.args = list(alpha = 0.7, size = 2, color = "grey50"),
#  xfill = "#CC79A7", ## fill for marginals on the x-axis
#  yfill = "#009E73", ## fill for marginals on the y-axis
  title = "Relationship between V_g and H2",
)

ggsave(filename =  "~/Documents/Cesar/git/quality_2022/H2_Var.png", plot = Plot1, dpi = 300, width = 6, height = 6)

head(data1)
Plot2 <- ggbetweenstats(
  data = data1,
  x = env, ## grouping/independent variable
  y = BLUE, ## dependent variables
  type = "robust", ## type of statistics
  xlab = "Loc", ## label for the x-axis
  ylab = "BLUP", ## label for the y-axis
  plot.type = "box", ## type of plot
  pairwise_comparisons = T, 
  p.adjust.method = "fdr",
  point.args = list(alpha = 0),
  outlier.tagging = TRUE, ## whether outliers should be flagged
  outlier.coef = 1.5, ## coefficient for Tukey's rule
  outlier.label = gen, ## label to attach to outlier values
  outlier.label.args = list(color = "grey"), ## outlier point label color
  title = "ADF",
  ## turn off messages
  ggtheme = ggplot2::theme_classic(), ## a different theme
  package = "yarrr", ## package from which color palette is to be taken
  palette = "info2", ## choosing a different color palette
  bf.message = F,
  results.subtitle = F
)

ggsave(filename =  "~/Documents/Cesar/git/quality_2022/box_adf.png", plot = Plot2, dpi = 300, width = 6, height = 6)


head(qual_BLUP3)
colnames(qual_BLUP3)
