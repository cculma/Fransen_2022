

summary(m3, coef = TRUE)$coef.fixed
wald.asreml(m3, ssType = "conditional", denDF = "numeric")

summary(m3)$varcomp

# variance components ST0
Var1 <- list()
for (i in 1:length(comp_var)) {
  data <- comp_var[[i]] %>% rownames_to_column(var = "comp") %>% select(c(1:2)) %>% slice(-4)
  V_G <- data[2,2]
  V_R <- sum(abs(data[,2]))
  H2 <- V_G/V_R
  D1 <- data.frame(V_G, V_R, H2)
  Var1[[length(Var1)+1]] <- D1
}
names(Var1) <- names(a3)
Var1 <-rbindlist(Var1, use.names=TRUE, fill=TRUE, idcol="trait")
Var2 <- Var1 %>% separate(1, c("loc", "year", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year), sep = "_", remove = T) 

write.csv(Var2, "~/Documents/Cesar/git/quality_2022/Pheno/Var2.csv", quote = F, row.names = F)


summary(SW_V)$varcomp
FA_1

data <- comp_var[[i]] %>% rownames_to_column(var = "comp") %>% select(c(1:2)) %>% slice(-4)