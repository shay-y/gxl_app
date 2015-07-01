data_m <- warpbreaks %>% select(y = breaks, x = tension)
alpha <- 0.05
design <- "Tukey"
type = "single-step"
S2_int <- 2
n_labs <- 2
n_genotype <- 6
summ_stats <- data_m %>% group_by(x) %>% summarise(means=mean(y),sds=sd(y),n=n())
name_vec <- summ_stats$x
mean_vec <- summ_stats$means
SD_vec   <- summ_stats$sds
N_vec    <- summ_stats$n

so <- gxl_adjust(design = "Tukey",type = "single-step",name_vec, mean_vec, SD_vec, N_vec, S2_int, n_labs, n_genotype)

pv <- so$summ$test$pvalues
pv_new <- so$summ_new$test$pvalues
tbl_ci  <- so$ci$confint
tbl_ci_new <- so$ci_new$confint

tbl_res <- cbind(pv,pv_new,tbl_ci,tbl_ci_new[,2:3])

plot(so$ci)
plot(so$ci_new)


# x1 <- o$co$ci
# x2 <- o$co$ci_new

cols <- 
plot_confint_glht(x1,x2, xlab = "Measure",
                  main = "Confidence Intervals",
                  cex.axis = 0.8,
                  cex_leg = 0.8,
                  cex.main = 0.97,
                  cex.subt = 0.9,
                  cex.lab = 0.85,
                  notch = 0.12,
                  text.font = 3,
                  font.axis = 3,
                  inset = 0.1,
                  ltys = c(1,1),
                  cols = cols) 
