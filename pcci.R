plot_pcci(tbl_pairs = tbl_pairs_temp)

plot_pcci <- function(tbl_pairs, title = "Confidence Intervals of Group Means", ylab = "Group means", xlab = "Group means Differences")
{
  cols = rev(c("#7570B3","#D95F02"))
  tbl_pairs_to_plot <- tbl_pairs %>% 
    gather( key = key, value = value, contains("pv"), contains("lwr"), contains("upr")) %>% 
    separate(key, c("measure","Adjustment"), sep = "_") %>% 
    mutate(Adjustment = ifelse(is.na(Adjustment),"Unadjusted","GxL\nAdjusted") %>% factor(levels = c("Unadjusted","GxL\nAdjusted"))) %>% 
    spread(key = measure, value = value) %>% 
    mutate(
      yend = (mean.t.x+mean.t.y)/2,
      xend = abs(diff),
      lwr  = ifelse(diff > 0, lwr, lwr+2*(-diff)),
      upr  = ifelse(diff > 0, upr, upr+2*(-diff)),
      Significance = factor(ifelse( lwr > 0,1,2), levels = c(1,2), labels = c("Significant","Not significant")))
  
  x_nudge   <- -max(tbl_pairs_to_plot$xend)/40
  err_bar_h <-  max(tbl_pairs_to_plot$xend)/28
  
  pcci <- 
    ggplot() +
    geom_segment(
      mapping = aes(x = x, y = y, xend = xend, yend = yend),
      data =  tbl_pairs_to_plot  %>% transmute(x = 0, y = mean.t.x, xend = xend, yend = yend),
      colour = gray(0.8),size = 0.8
    )+
    geom_segment(
      mapping = aes(x = x, y = y, xend = xend, yend = yend),
      data =  tbl_pairs_to_plot  %>% transmute(x = 0, y = mean.t.y, xend = xend, yend = yend),
      colour = gray(0.8),size = 0.8
    ) +
    geom_vline(xintercept = 0,colour = grey(0.5)) +
    geom_point(
      aes(x = x, y = value),
      data = tbl_pairs %>% transmute(x = 0, value = mean.t.x) %>% distinct(),
      colour = gray(0.3),
      shape = 3) + 
    theme_minimal() +
    coord_fixed(ratio = 2) +
    xlab(xlab) + ylab(ylab) +  ggtitle(title) +
    geom_errorbarh( 
    aes(x = x, y = y, xmin = xmin ,xmax = xmax,color = Significance, alpha = Adjustment, height = ifelse(Adjustment == "GxL\nAdjusted",0,err_bar_h)),
    data = tbl_pairs_to_plot %>% transmute(x = xend, y = yend, xmin = lwr, xmax = upr,Adjustment,Significance)) +
    scale_color_manual(values = cols,  drop = F) + 
    scale_alpha_manual(values = c(1,1),labels = c("","")) +
    guides(
      color = guide_legend(order = 1, reverse = F,title = ""),
      alpha = guide_legend(override.aes = list(alpha = 0),
                           order = 2,
                           title = "Outer segments are GxL Adjusted",
                           title.theme = element_text(size = 14*0.8,angle = 0))) +
    theme(legend.position = "bottom", legend.box = "horizontal")
  
  tbl_grps <- union(
    tbl_pairs_temp %>% select(grp = grp1, mean = mean.t.x) %>% distinct(),
    tbl_pairs_temp %>% select(grp = grp2,mean = mean.t.y) %>% distinct()
  )
  
  pcci <-  pcci +
    geom_text(
      aes(x = 0, y = mean, label = grp),
      data = tbl_grps,
      colour = gray(0.2), size = 12*(5/14)*0.9,
      check_overlap = T,hjust = "right",nudge_x = x_nudge)
  
  pcci <- pcci +
    geom_point(
      aes(x = x, y = y),
      data = tbl_pairs_to_plot %>% transmute(x = xend, y = yend,name_pair),
      color = gray(0.5))
  return(pcci)
}


