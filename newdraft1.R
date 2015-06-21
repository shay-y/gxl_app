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
