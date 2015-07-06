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
                       type = c("single-step","none"),alpha = 0.05)
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

# install.packages("RColorBrewer")
# library(RColorBrewer)
# cols <- brewer.pal(4,"Paired")[c(2,4)]
plot_confint_glht <- function (x1,x2, xlab = "Measure",
                               main = "Confidence Intervals",
                               cex.axis = 0.9,
                               cex_leg = 1,
                               cex.main = 1,
                               cex.subt = 0.95,
                               cex.lab = 1,
                               notch = 0.12,
                               text.font = 3,
                               font.axis = 3,
                               inset = 0.1,
                               ltys = c(1,1),
                               cols = c("#1F78B4", "#33A02C"))
{
  old_mar <- par()$mar # c(5, 4, 4, 2) + 0.1
  par(mar = c(5, 5.5, 4, 2) + 0.1)
  
  xi1 <- x1$confint
  xi2 <- x2$confint
  ylab <- unlist(lapply(strsplit(rownames(xi1),split = " - "),paste0,collapse = " -\n"))
  xi <- rbind(xi1,xi2)
  xrange <- c(min(xi[, "lwr"]), max(xi[, "upr"]))
  if (!is.finite(xrange[1])) 
    xrange[1] <- min(xi[, "Estimate"])
  if (!is.finite(xrange[2])) 
    xrange[2] <- max(xi[, "Estimate"])
  yvals <- c( (nrow(xi1):1)+notch, (nrow(xi1):1)-notch )
  xlim <- xrange
  ylim <- c(0.3, nrow(xi1) + 0.5)
  plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2), type = "n", 
       axes = FALSE, xlab = NA,sub = NA, ylab = "", xlim = xlim, ylim = ylim)
  box(col="grey50")
  axis(1, cex.axis = cex.axis,font = 1)
  axis(2, at = nrow(xi1):1, labels = ylab, las = 1,  cex.axis = cex.axis,font = 1)
  # abline(h = yvals, lty = 1, lwd = 1, col = "lightgray")
  abline(v = 0, lty = 3, lwd = 1,  col = "grey10")
  left <- xi[, "lwr"]
  left[!is.finite(left)] <- min(c(0, xlim[1] * 2))
  right <- xi[, "upr"]
  right[!is.finite(right)] <- max(c(0, xlim[2] * 2))
  col_both <- rep(cols,each=nrow(xi1))
  lty_both <- rep(ltys,each=nrow(xi1))
  segments(left, yvals, right, yvals, col = col_both, lty = lty_both)
  points(xi[, "lwr"], yvals, pch = "(", col = col_both)
  points(xi[, "upr"], yvals, pch = ")", col = col_both)
  points(xi[, "Estimate"], yvals, pch = 20, col = col_both)
  if (attr(x1, "type") == "adjusted") {
    subt <- paste(format(100 * attr(xi1, "conf.level"), 
                         2), "% family-wise confidence level", sep = "")
  } else {
    subt <- paste(format(100 * attr(xi1, "conf.level"), 
                         2), "% confidence level", sep = "")
  }
  legend("bottom",inset = 0.02,c("unadjusted","GxL adjusted"), horiz = T, lty = ltys, col = cols,
         box.col = "grey10", text.font = 3, cex = cex_leg)
  title(main,cex.main = cex.main, line= 2)
  mtext(text = subt,side = 3, line = 1,cex = cex.subt, font = 1)
  title(xlab = xlab, cex.lab = cex.lab)
  box("figure",col="grey50" )
  par(mar = old_mar)
}



