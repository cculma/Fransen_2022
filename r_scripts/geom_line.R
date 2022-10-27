
head(BLUP2)
BLUP2 <- BLUP2 %>% rownames_to_column("gen")
summary(BLUP2$OR_2018)
levels(BLUP2$year)
BLUP2$year <- factor(BLUP2$year, levels=c('2018', '2019', '2020', '2021'))

BLUP2.2 <- BLUP2 %>% dplyr::filter(gen %in% c(201,202,2,4,140,22,112,58)) %>% separate(col = env, into = c("loc", "year"))
str(BLUP2.2)

BLUP2.2 <- BLUP2.2 %>% gather(key = "env", value = "predicted.value", 2:12) %>% separate(col = env, into = c("loc", "year"))

head(BLUP2.2)
ggplot(data=BLUP2.2, aes(x=year, y=predicted.value, group=gen)) +
  geom_line(aes(color=gen)) + theme_minimal(base_family = "Arial", base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + 
  scale_color_brewer(palette="Set1") +
  geom_point(aes(color=gen)) + facet_grid( ~ loc, scales = "free_y", space = "free") + labs(title = "CP-subsample", x = "", y = "BLUPs")


ggplot(BLUP2, aes(x = env, y = predicted.value, fill = env)) + geom_boxplot(alpha = 0.6, outlier.shape = NA, width=0.6, position = position_dodge(width=0.8, preserve = "single"))  + theme_bw(base_family = "Arial") + theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(title = "ADF", y = "BLUP", x = "") + scale_fill_manual(values = mycolors1)

nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
"#9FF67F"



mycolors1 <- c("#A6CEE3", "#4791C2", "#59A1A3", "#A5D880", "#4CAC3E", "#7BE031", "#60E42F", "#CF7208", "#F27D4D", "#FDAB4D", "#FF7F00")
