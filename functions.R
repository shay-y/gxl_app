mock_data <- function(name_vec,mean_vec,SD_vec,N_vec)
{
  lasts <- cumsum(N_vec)
  x <- as.factor(rep(name_vec,N_vec))
  y          <- rep(0,sum(N_vec))
  y[lasts]   <- 1/sqrt(2)
  y[lasts-1] <- -1/sqrt(2)
  a <- rep(SD_vec*sqrt(N_vec-1),N_vec)
  y <- y*a + rep(mean_vec,N_vec)
  return(data.frame(y = y ,x = x))
}

gxl_adjust <- function(name_vec, mean_vec, SD_vec, N_vec, S2_int, n_labs, n_genotype,
                       design = c("Tukey","Dunnet"),
                       type = c("single-step","none"),alpha)
{
  data_m <- mock_data(name_vec, mean_vec, SD_vec, N_vec)
  
  fit  <- lm(y ~ x, data = data_m)
  glh  <- glht(fit, linfct = mcp(x = design))
  summ_fit <- summary(fit)
  summ_glh <- summary(glh, test = adjusted(type))
  se_pairs <- summ_glh$test$sigma
  sigma    <- summ_fit$sigma
  recp_df_pairs <- (se_pairs/sigma)^2
  
  se_pairs_new <- sqrt(se_pairs^2+2*S2_int)
  sigma_new <- ( se_pairs_new/sqrt(recp_df_pairs) )[1]
  vcov_new <- sigma_new^2 * summ_fit$cov.unscaled
  
  # Satterthwaite approximation
  ni_num <-  ( sigma^2 * recp_df_pairs + 2*S2_int )^2  
  ni_denum <-  (sigma^2 * recp_df_pairs)^2 / fit$df.residual + (2*S2_int)^2 /((n_labs-1)*(n_genotype-1)) 
  ni <- ni_num/ni_denum
  
  glh_new <- glht(fit, linfct = mcp(x = design), vcov. = vcov_new , df = floor(ni[1])) # check df
  summ_glh_new <- summary(glh_new, test = adjusted(type))
  
  if (type=="none")
    calpha <- univariate_calpha()
  else
    calpha <- adjusted_calpha()
  ci     <- confint(glh,level = 1 - alpha , calpha)
  ci_new <- confint(glh_new,level = 1 - alpha , calpha)

  return(list(summ = summ_glh, summ_new = summ_glh_new, ci = ci, ci_new = ci_new))
}

get_res_table <- function(co)
{
  pv <- co$summ$test$pvalues
  pv_new <-co$summ_new$test$pvalues
  tbl_ci  <- co$ci$confint
  tbl_ci_new <- co$ci_new$confint
  
  ci_l_new <- ifelse(is.null(tbl_ci_new[,2]),NA,tbl_ci_new[,2])
  ci_u_new <- ifelse(is.null(tbl_ci_new[,3]),NA,tbl_ci_new[,3])
  
  tbl_res <- cbind(pv,pv_new,tbl_ci,ci_l_new,ci_u_new)
  colnames(tbl_res) <- c("p_value","p_value_adj", "est", "ci_lwr", "ci_upr",  "ci_lwr_adj", "ci_upr_adj")
  return(tbl_res)
}