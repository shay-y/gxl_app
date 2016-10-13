plot_pcci <- function(pcci_obj, title = "Pairwise Comparisons Confidence Intervals",
                     ylab = "Measure", labels = FALSE, colors = NULL, error_bar_height = NULL)
{
  if (is.null(colors))
    cols = rev(c("#7570B3","#D95F02"))
  else
  {
    if (length(colors) ==2)
      cols <- colors
    else
      stop("length(colors) not equals 2")
  }
  
  tbl_pairs <- pcci_obj$tbl_pairs
  tbl_summaries <- pcci_obj$tbl_summaries
  
  coef_x_scale=1
  
  suppressWarnings(
    tbl_pairs_to_plot <- tbl_pairs %>% 
      gather(key = key, value = value,contains("pv"),contains("lwr"),contains("upr")) %>% 
      separate(key,c("measure","Adjustment"),sep = "_") %>% 
      mutate(Adjustment = factor(ifelse(is.na(Adjustment),"Unadjusted","GxL\nAdjusted"),levels = c("Unadjusted","GxL\nAdjusted"))) %>% 
      spread(key = measure, value = value) %>% 
      mutate(
        yend = (y.x+y.y)/2,
        xend = abs(diff)*coef_x_scale,
        lwr = ifelse(diff>0,lwr,lwr+2*(-diff)),
        upr = ifelse(diff>0,upr,upr+2*(-diff)),
        Significance = factor(ifelse(lwr>0,1,2),levels = c(1,2),labels = c("Significant","Not significant")))
  )
 
  x_nudge <- -max(tbl_pairs_to_plot$xend)/40
  
  if (is.null(error_bar_height))
    err_bar_h = max(tbl_pairs_to_plot$xend)/28
  else
    err_bar_h <- error_bar_height
  
  pcci <- 
    ggplot() +
    geom_segment(
      mapping = aes(x = x, y = y, xend = xend, yend = yend),
      data =  tbl_pairs_to_plot  %>% transmute(x = x.x, y = y.x, xend = xend, yend = yend),
      colour = gray(0.8),size = 0.8
    )+
    geom_segment(
      mapping = aes(x = x, y = y, xend = xend, yend = yend),
      data =  tbl_pairs_to_plot  %>% transmute(x = x.y, y = y.y, xend = xend, yend = yend),
      colour = gray(0.8),size = 0.8
    ) +
    geom_vline(xintercept = 0,colour = grey(0.5)) +
    geom_point(
      aes(x = x, y = value),
      data = tbl_summaries %>% transmute(x , value = y),
      colour = gray(0.3),
      shape = 3) + 
    theme_minimal() +
    coord_fixed(ratio = 2)+
    xlab("|difference|") +
    ylab(ylab)+
    ggtitle(title) +
    
    geom_errorbarh( 
    aes(x = x, y = y, xmin = xmin ,xmax = xmax,color = Significance, alpha = Adjustment, height = ifelse(Adjustment == "GxL\nAdjusted",0,err_bar_h)),
    data = tbl_pairs_to_plot %>%
      transmute(x = xend, y = yend, xmin = lwr*coef_x_scale, xmax = upr*coef_x_scale,Adjustment,Significance),
    position =  position_stack()) + 
    scale_color_manual(values = cols,  drop = F) + 
    scale_alpha_manual(values = c(1,1),labels = c("","")) +
    guides(
      color = guide_legend(order = 1, reverse = F,title = ""),
      alpha = guide_legend(override.aes = list(alpha = 0),
                           order = 2,
                           title = "outer segments are\nGxL Adjusted",
                           title.theme = element_text(size = 14*0.8,angle = 0))) +
    theme(legend.position = "bottom", legend.box = "horizontal")
  
  
  # if (labels)
  #   pcci <-  pcci +
  #     geom_label_repel(
  #       aes(x = abs(xend), y = yend, label = format(xend,digits = 3,nsmall = 3,scientific = 4)),
  #       data = tbl_pairs_to_plot %>% filter(Adjustment == "Unadjusted"),colour = gray(0.2))
  
  pcci <-  pcci +
    geom_text(
      aes(x = x, y = y, label = group_name),
      data = tbl_summaries,
      colour = gray(0.2), size = 12*(5/14)*0.9,
      check_overlap = T,hjust = "right",nudge_x = x_nudge)
  
  pcci <- pcci +
    geom_point(
      aes(x = `|difference|`, y = y),
      data = tbl_pairs_to_plot %>% transmute(`|difference|` = xend, y = yend,name_pair),
      color = gray(0.5))
  return(pcci)
}


