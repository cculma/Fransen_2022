rm(list = ls())

library(metan)
library(ggplot2)
library(tidyverse)
library(ggrepel)

a <- ge_plot(data_ge, ENV, GEN, GY)
head(data_ge)

names(SC1)
names(ST1)
a6 <- ST1[[6]]


a6 <- ST1[[1]]
a6 <- ST1[[3]]
a6 <- ST1[[11]]

a6 <- as.data.frame(a6)
# a6 <- a6 %>% separate(2, c("loc", "year"), sep = "_", remove = F, convert = FALSE, extra = "merge")

a6 <- as_tibble(a6)
a6$env <- as.factor(a6$env)
a6$gen <- as.factor(a6$gen)
# a6$loc <- as.factor(a6$loc)
head(a6)
class(a6)
str(a6)

two.way <- aov(predicted.value ~ gen + env, data = a6)
summary(two.way)

# b <- ge_plot(a6, env, gen, predicted.value) 
# b <- ge_winners(a6, env, gen, resp = predicted.value, type = "ranks")
# b <- b %>% rownames_to_column(var = "gen") %>% spread(key = ENV, value = predicted.value)
# b <- make_mat(a6, env, gen, predicted.value)
# b <- ge_effects(a6, env, gen, predicted.value, type = "ge") # important
# ?ge_effects
# plot(b)
# b <- superiority(a6, env, gen, predicted.value)

b01 <- superiority(a6, env, gen, predicted.value)
b03 <- superiority(a6, env, gen, predicted.value)
b11 <- superiority(a6, env, gen, predicted.value)

colnames(b01[["predicted.value"]]$environments)[2:4] <- paste0(colnames(b01[["predicted.value"]]$environments)[2:4], "_ADF")
colnames(b03[["predicted.value"]]$environments)[2:4] <- paste0(colnames(b03[["predicted.value"]]$environments)[2:4], "_aNDF")
colnames(b11[["predicted.value"]]$environments)[2:4] <- paste0(colnames(b11[["predicted.value"]]$environments)[2:4], "_Lignin")

SEnv1 <- inner_join(b01[["predicted.value"]]$environments, b03[["predicted.value"]]$environments, by = "ENV") %>% inner_join(., b11[["predicted.value"]]$environments, by = "ENV") 

colnames(b01[["predicted.value"]]$index)[2:8] <- paste0(colnames(b01[["predicted.value"]]$index)[2:8], "_ADF")
colnames(b03[["predicted.value"]]$index)[2:8] <- paste0(colnames(b03[["predicted.value"]]$index)[2:8], "_aNDF")
colnames(b11[["predicted.value"]]$index)[2:8] <- paste0(colnames(b11[["predicted.value"]]$index)[2:8], "_Lignin")

SInx1 <- inner_join(b01[["predicted.value"]]$index, b03[["predicted.value"]]$index, by = "GEN") %>% inner_join(., b11[["predicted.value"]]$index, by = "GEN") 

colnames(SInx1)
colnames(SEnv1)
SEnv2 <- SEnv1[,c(1,2,5,8,1,3,6,9,1,4,7,10)]

SInx2 <- SInx1[,c(1,2,9,16,1,3,4,10,11,17,18,1,5,6,12,13,19,20,1,7,8,14,15,21,22)]

write.csv(SInx2, "~/Documents/Cesar/git/quality_2022/Pheno/SInx1.csv", quote = F, row.names = F)
write.csv(SEnv2, "~/Documents/Cesar/git/quality_2022/Pheno/SEnv1.csv", quote = F, row.names = F)

out <- ge_acv(a6, env, gen, predicted.value)

b1 <- b[["predicted.value"]][["index"]][,c(1,2)]
b2 <- out[["predicted.value"]][,c(1,2)]
b3 <- inner_join(b1, b2, by = "GEN")


gge_eff <- ge_effects(a6, env, gen, predicted.value, type = "ge")
plot(gge_eff)

mod <- gge(a6, env, gen, predicted.value)
predict(mod)

# options(ggrepel.max.overlaps = Inf)
names(ST1)

gge_model <- gge(ST1[[10]], env, gen, predicted.value, centering = 2, scaling = 1, svp = 1)

plot(gge_model, type = 9, sel_gen1 = "201", sel_gen2 = "202", col.gen = "black", col.env = "blue", col.alpha = 0.7,  axis_expand = 1.2, size.text.env = 2, size.text.gen = 2, size.text.win = 2, size.stroke = 0.2, shape.gen = 21, shape.env = 23, plot_theme = theme_metan_minimal(), title = F) + theme(legend.position = "none")

model <- gge_model[[var]]
str(gge_model)

gge_model$predicted.value$labelgen
gge_model$predicted.value$labelenv
gge_model$predicted.value$coordgen[, c(1, 2)]
gge_model$predicted.value$coordenv[, c(1, 2)]

coord_gen <- model$coordgen[, c(1, 2)]
coord_env <- model$coordenv[, c(1, 2)]

plotdata <- data.frame(rbind(data.frame(gge_model$predicted.value$coordgen[, c(1, 2)],
                                        type = "genotype",
                                        label = gge_model$predicted.value$labelgen),
                             data.frame(gge_model$predicted.value$coordenv[, c(1, 2)],
                                        type = "environment",
                                        label = gge_model$predicted.value$labelenv)))


B_plot <- list()
for (i in 1:length(ST1)) {
  a6 <- ST1[[i]]
  a6 <- as.data.frame(a6)
  a6 <- as.tibble(a6)
  a6$env <- as.factor(a6$env)
  a6$gen <- as.factor(a6$gen)
  lev3 <- names(ST1)[i]
  gge_model <- gge(a6, env, gen, predicted.value, centering = 2, scaling = 1, svp = 1)
  g1 <- plot(gge_model, type = 1, col.gen = "black", col.env = "blue", col.line = "blue", col.alpha = 0.6, plot_theme = theme_metan_minimal(), title = F, axis_expand = 1.2) + theme(legend.position = "none") + labs(title = paste(lev3, "ST1"))
  B_plot[[length(B_plot)+1]] <-  g1
}

names(B_plot) <- names(ST1)

B_plot[[7]] + B_plot[[6]] + B_plot[[1]] + B_plot[[3]] + plot_layout(ncol = 2)
B_plot[[4]] + B_plot[[9]] + B_plot[[11]] + B_plot[[15]] + plot_layout(ncol = 2)
B_plot[[5]] + B_plot[[14]] + B_plot[[10]] + B_plot[[12]] + plot_layout(ncol = 2)
B_plot[[8]] + B_plot[[16]] + B_plot[[2]] + B_plot[[13]] + plot_layout(ncol = 2)

B_plot[[8]] + scale_shape_manual(values = c("201" = "red", "202" = "red"))


for (i in 1:length(B_plot)) {
  ggsave(filename = paste0("~/Documents/Cesar/git/quality_2022/Pheno/biplot/", names(B_plot)[i], ".pdf"), plot = B_plot[[i]], width = 6, height = 6, device = cairo_pdf)
}

str(FA_1)
FA_1$G.param$`fa(env, 1):gen`

#################

# biplot all traits

names(SC1)
a6 <- ST1[c(11,1,3)]
a6 <- rbindlist(a6, use.names=TRUE, fill=TRUE, idcol="trait")

head(a6)
class(a6)
a6 <- as.data.frame(a6)
a6 <- a6 %>% separate(3, c("loc", "year"), sep = "_", remove = F, convert = FALSE, extra = "merge")
colnames(a6)[1:5]
a6[colnames(a6)[1:5]] <- lapply(a6[colnames(a6)[1:5]], factor)
a6 <- as.tibble(a6)
str(a6)
a6 <- a6 %>% spread(key = trait, value = predicted.value)

b1 <- b[["predicted.value"]][["index"]][,c(1,2)]
b2 <- out[["predicted.value"]][,c(1,2)]
b3 <- inner_join(b1, b2, by = "GEN")
ggplot(b3, aes(x=Y, y=ACV, label = GEN)) + geom_point(alpha = 0.6) + geom_text_repel() + theme_bw(base_family = "Arial", base_size = 12) + labs(x = "Mg mean", y = "CV")


mod <- waasb(a6,
             env = loc,
             gen = gen,
             rep = year,
             resp = c(ADF, aNDF, Lignin))

FAI <- fai_blup(mod, use_data = "blup",
                SI = 15,
                DI = c(200,200),
                UI = c(200,200))



gge_eff <- ge_effects(a6, env, gen, predicted.value, type = "ge")
head(a6)
head(ST3.1)

gge_eff <- ge_cluster(ST3.1, trait, gen, predicted.value, nclust = 5, distmethod = 'manhattan', clustmethod = 'ward.D2')
plot(gge_eff)
plot(gge_eff, nclust = 4)
?ge_cluster

setwd("~/Documents/Cesar/git/quality_2022/Pheno/")
library(modelsummary)
?modelsummary
modelsummary(two.way)
AOV1 <- get_estimates(two.way, output = "AOV.docx")
modelsummary(two.way, output = "AOV.docx")
?datasummary_correlation
datasummary_correlation(ST3.5,
                        output = "ST3.5.docx")

datasummary_correlation(ST3.4,
                        output = "ST3.4.docx")

datasummary_correlation(ST3.3,
                        output = "ST3.3.docx")

datasummary_correlation(ST3.2,
                        output = "ST3.2.docx")
