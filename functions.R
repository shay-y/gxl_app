# install.packages(c("dplyr","ggplot2","ggrepel"))
suppressWarnings(
  {
    library(dplyr,warn.conflicts = F)
    library(ggplot2,warn.conflicts = F)
    library(ggrepel,warn.conflicts = F)
    library(tidyr,warn.conflicts = F)
  }
)

multiplicity_adjustment_methods = c("none","TukeyHSD","BH_selected")
calculate_pcci <- function(data, factor_col, value_col, alpha = 0.05, multiplicity_adjustment = multiplicity_adjustment_methods,
                          s2_ratio = NULL, s2_interaction = NULL, n_levels_rand = NULL, n_levels_fixed = NULL)
{
  multiplicity_adjustment<- match.arg(multiplicity_adjustment)
  
  ## summaries data
  tbl_summaries <- data %>% tbl_df() %>% 
    transmute_(group_name = factor_col,value = value_col) %>% 
    group_by(group_name) %>% 
    summarise(mean = mean(value,na.rm = T), sd = sd(value,na.rm = T), n = n()) %>% 
    mutate(
      df = sum(n)-n(),
      s2_pooled = sum(sd^2*(n-1))/df,
      s2_interaction =
        if (!is.null(s2_ratio))
        {
          if (is.null(s2_interaction))
            s2_pooled*s2_ratio
          else stop("Entered both s2_ratio and s2_interaction. input onel only.")
        } else
        {
          if (is.null(s2_interaction))
            NA
          else 
            s2_interaction
        },
      df_gxl = 
        if (any(is.null(n_levels_rand),is.null(n_levels_fixed),is.null(s2_interaction),is.na(s2_interaction)))
          NA
        else
          (s2_pooled + 2*s2_interaction )^2  /( s2_pooled^2/(sum(n)-n()) + (2*s2_interaction)^2 /( (n_levels_rand-1)*(n_levels_fixed-1) ))
    ) %>% 
    arrange(desc(mean)) %>%
    mutate(x = 0, y = mean) %>% 
    add_rownames(var = "group_id")
  
  ## generate indices pairs table:
  tbl_indices_pairs <- nrow(tbl_summaries) %>% 
    combn(2) %>% 
    t() %>% 
    as.data.frame(stringsAsFactors=FALSE) %>% 
    tbl_df() %>% 
    transmute(group_id1 = as.factor(V1),group_id2 = as.factor(V2))

  ## generate pairs table and calculate pvalues and ci's:
  suppressWarnings(
    {tbl_pairs_join <- tbl_indices_pairs %>% 
      inner_join(tbl_summaries,by = c("group_id1"="group_id")) %>% 
      inner_join(tbl_summaries,c("group_id2"="group_id","s2_pooled","df","df_gxl"))}
  )
  
  tbl_pairs <- tbl_pairs_join %>% 
    mutate(
      pair_id = paste0(group_id1,"-",group_id2),
      group1  = group_name.x,
      group2  = group_name.y,
      name_pair = paste(group_name.x,"-",group_name.y),
      diff  = abs(mean.x-mean.y),                    
      se = sqrt(s2_pooled*(1/n.x+1/n.y)),
      se_gxl = sqrt(s2_pooled*(1/n.x+1/n.y)+2*s2_interaction.x), 
      stat = diff/se,
      stat_gxl =diff/se_gxl) %>% 
      {
        if (multiplicity_adjustment == "TukeyHSD")
        {
          mutate(
            .,
            pv = ptukey(q = stat,nmeans = nrow(tbl_summaries) , df = df, lower.tail = FALSE) %>% pmin(1),
            pv_gxl = ptukey(q = stat_gxl, nmeans = nrow(tbl_summaries), df = df_gxl ,lower.tail = FALSE) %>% pmin(1),
            lwr  = diff - qtukey(p = 1-alpha, nmeans = nrow(tbl_summaries), df = df)*se/sqrt(2),
            upr  = diff + qtukey(p = 1-alpha, nmeans = nrow(tbl_summaries), df = df)*se/sqrt(2),
            lwr_gxl = diff - qtukey(p = 1-alpha, nmeans = nrow(tbl_summaries), df = df_gxl)*se_gxl/sqrt(2),
            upr_gxl = diff + qtukey(p = 1-alpha, nmeans = nrow(tbl_summaries), df = df_gxl)*se_gxl/sqrt(2))
        } else
          if (multiplicity_adjustment == "BH_selected")
          {
            mutate(
              .,
              pv = 2*pt(q = stat,df = df, lower.tail = FALSE) %>% pmin(1),
              pv_gxl = 2*pt(q = stat_gxl,df = df_gxl, lower.tail = FALSE) %>% pmin(1),
              pv = p.adjust(pv,"BH"),
              pv_gxl = p.adjust(pv_gxl,"BH"),
              R <- sum(pv <= alpha),
              R_gxl <- sum(pv_gxl <= alpha),
              lwr  = diff - qt(1-alpha/2 * R/length(pv),df = df)*se,
              upr  = diff + qt(1-alpha/2 * R/length(pv),df = df)*se,
              lwr_gxl = diff - qt(1-alpha/2 * R_gxl/length(pv_gxl),df = df_gxl)*se_gxl,
              upr_gxl = diff + qt(1-alpha/2 * R_gxl/length(pv_gxl),df = df_gxl)*se_gxl)
          } else
            if (multiplicity_adjustment == "none")
            {
              mutate(
                .,
                pv = 2*pt(q = stat,df = df, lower.tail = FALSE) %>% pmin(1),
                pv_gxl = 2*pt(q = stat_gxl,df = df_gxl, lower.tail = FALSE) %>% pmin(1),
                lwr  = diff - qt(1-alpha/2,df = df)*se,
                upr  = diff + qt(1-alpha/2,df = df)*se,
                lwr_gxl = diff - qt(1-alpha/2,df = df_gxl)*se_gxl,
                upr_gxl = diff + qt(1-alpha/2,df = df_gxl)*se_gxl)
            } else stop("Select valid option for multiplicity_adjustment")
      } %>% 
              
        
    transmute(name_pair, diff, pv, pv_gxl, lwr, upr, lwr_gxl, upr_gxl, x.x, x.y, y.x, y.y)
  return(list(tbl_pairs = tbl_pairs,tbl_summaries = tbl_summaries))
}


plot_pcci <- function(pcci_obj, title = "Pairwise Comparisons Confidence Intervals",
                     ylab = "Measure", labels = TRUE, y_labels = FALSE, colors = NULL,
                     error_bar_height = NULL, axis_labels_nudge = NULL)
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
      mutate(Adjustment = ifelse(is.na(Adjustment),"Unadjusted","Interaction\nAdjusted")) %>% 
      spread(key = measure, value = value) %>% 
      mutate(
        yend = (y.x+y.y)/2,
        xend = diff*coef_x_scale,
        Significance = factor(ifelse(lwr>0,1,2),labels = c("Significant","Not significant")))
  )
  
  if (is.null(axis_labels_nudge))
    x_nudge <- -max(tbl_pairs_to_plot$diff)/40
  else
    x_nudge <- axis_labels_nudge
  
  if (is.null(error_bar_height))
    err_bar_h = max(tbl_pairs_to_plot$diff)/28
  else
    err_bar_h <- error_bar_height
  
  pcci <- ggplot() +
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
    ggtitle(title)
  
  add_adjust <- !is.na(tbl_summaries$s2_interaction[1])
  
  if(add_adjust)
  {
    if (err_bar_h == 0)
      err_bar_h <- max(tbl_pairs_to_plot$diff)/28
    pcci <- pcci + geom_errorbarh( 
      aes(x = x, y = y, xmin = xmin ,xmax = xmax,color = Significance, linetype = Significance, alpha = Adjustment),
      data = tbl_pairs_to_plot %>%
        transmute(x = xend, y = yend, xmin = lwr*coef_x_scale, xmax = upr*coef_x_scale,Adjustment,Significance),
      height = err_bar_h) + 
      scale_color_manual(values = cols) + 
      scale_alpha_manual(values = c(1,1),labels = c("","")) +
      guides(
        color = guide_legend(order = 1),
        linetype = guide_legend(order = 1),
        alpha = guide_legend(override.aes = list(alpha = 0),
                             order = 2,
                             title = "outer segments are\nInteraction Adjusted",
                             title.theme = element_text(size = 12*0.8,angle = 0)))
  } else 
    pcci <- pcci + geom_errorbarh( 
      aes(x = x, y = y, xmin = xmin ,xmax = xmax,color = Significance, linetype = Significance),
      data = tbl_pairs_to_plot %>%
        filter(Adjustment == "Unadjusted") %>% 
        transmute(x = xend, y = yend, xmin = lwr*coef_x_scale, xmax = upr*coef_x_scale,Adjustment,Significance),
      height = err_bar_h
    )
  
  if (labels)
  {
    pcci <-  pcci +
      geom_label_repel(
        aes(x = abs(xend), y = yend, label = format(xend,digits = 3,nsmall = 3,scientific = 4)),
        data = tbl_pairs_to_plot %>% filter(Adjustment == "Unadjusted"),colour = gray(0.2))
  }
  
  if (y_labels)
    pcci <-  pcci +
    geom_label_repel(
      aes(x = x, y = y, label = paste0(group_name," : ", format(y,digits = 3,nsmall = 3,scientific = 4))),
      data = tbl_summaries,
      colour = gray(0.2),segment.size = 0.75, segment.color = "#652828",nudge_x = x_nudge,arrow = arrow(length = unit(0.01, 'npc'))) 
  else
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


