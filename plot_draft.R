bw = 0.5; jw = 0.1

source("functions.R")
glucose <- read.csv("Simplified IPGTT Glucose response AUC - example.csv")
ggplot(data = glucose) + aes(x = genotype, y = glucose.AUC) + geom_boxplot(width = bw, outlier.shape = NA) +
  geom_jitter(width = jw) + ylab("Glucose response AUC") + ggtitle("IMPC data")


ppci3 <- calculate_pcci(data =  glucose, factor_col = "genotype",
                        value_col  = "glucose.AUC",
                        s2_interaction =   4114.557,
                        n_levels_rand = 3,
                        n_levels_fixed = 7)

library(knitr)

kable(ppci3$tbl_summaries[2:7],caption = "Groups summaries")
kable(ppci3$tbl_pairs[1:8],caption = "Pairwise comparisons")

plot_pcci(ppci3,ylab = "glucose AUC (minutes/mmol/L)",labels = F,title = "Mean Glucose Tolerance:\nComparisons Between All Genotypes Pairs ")



install.packages(c("dplyr","ggplot2","ggrepel","tidyr","DT","shinyjs","stringr"))

