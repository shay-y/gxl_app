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
                               cols = c("#1F78B4", "#33A02C"),
                               lwd = 2.8,
                               lend = 0)
{
  old.par <- par(no.readonly = TRUE) # c(5, 4, 4, 2) + 0.1
  par(mar = c(5.5, 5.5, 4, 2) + 0.1)
  par(bg = NA)
  
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
  
  xrange
  plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2), type = "n", 
       axes = FALSE, xlab = NA,sub = NA, ylab = "", xlim = xlim, ylim = ylim)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "white")
  
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
  arrows
  segments(left, yvals, right, yvals, col = col_both, lty = lty_both,lwd=lwd,lend=lend)
  # points(xi[, "lwr"], yvals, pch = "(", col = col_both,cex=1.5)
  # points(xi[, "upr"], yvals, pch = ")", col = col_both,cex=1.5)
  points(xi[, "Estimate"], yvals, pch = 20, col = col_both,cex=1.5)
  if (attr(x1, "type") == "adjusted") {
    subt <- paste(format(100 * attr(xi1, "conf.level"), 
                         2), "% family-wise confidence level", sep = "")
  } else {
    subt <- paste(format(100 * attr(xi1, "conf.level"), 
                         2), "% confidence level", sep = "")
  }
  title(main,cex.main = cex.main, line= 2)
  mtext(text = subt,side = 3, line = 1,cex = cex.subt, font = 1)
  title(xlab = xlab, cex.lab = cex.lab)
  # box("figure",col="lightgray" )
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom",c("unadjusted","GxL adjusted"), horiz = T, lty = ltys, col = cols,
         box.col = "grey10", text.font = 3, cex = cex_leg,lwd=lwd,
         xpd = TRUE,inset = c(0,0), bty = "n")
  par(old.par)
}

tooltip_pv <- function(x)
{
  if(is.null(x)) return(NULL)
  if(is.null(x$pair_id)) return(NULL)
  row <- tbl_points[tbl_points$pair_id == x$pair_id,]
  paste0('<style type="text/css">
         .tg  {border-collapse:collapse;border-spacing:0;border:none;}
         .tg td{font-family:Arial, sans-serif;font-size:14px;padding:3px ;border:none;border-width:0px;overflow:hidden;word-break:normal;}
         .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:3px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
         .tg .tg-vkoua{font-family:"Lucida Console", Monaco, monospace !important;;color:#1f78b4} 
         .tg .tg-vkoad{font-family:"Lucida Console", Monaco, monospace !important;;color:#33a02c}
         .tg .tg-wja7{font-family:"Lucida Console", Monaco, monospace !important;;color:#1f78b4;text-align:center}
         .tg .tg-slkj{font-family:"Lucida Console", Monaco, monospace !important;;text-align:center}
         .tg .tg-a3u1{font-family:"Lucida Console", Monaco, monospace !important;;color:#33a02c;text-align:center}
         </style>
         <table class="tg">
         <tr>
         <th class="tg-slkj" colspan="2">P-values</th>
         </tr>
         <tr>
         <td class="tg-a3u1">Adj.&nbsp;&nbsp;</td>
         <td class="tg-vkoad">
         ',format(row$p_value_adj,digits = 4),'</td>
         </tr>
         <tr>
         <td class="tg-wja7">Unadj.</td>
         <td class="tg-vkoua">
         ',format(row$p_value,digits = 4),'</td>
         </tr>
         </table>'
      )
}
  
plot_diagram <- function(tbl_singles,tbl_pairs,alpha)
{
  inset_x <- 2
  inset_y <- 1
  cols <- c("#BDBDBD","#969696","#737373","#525252","#252525") #brewer.pal(n=9,"Greys")[4:8] 
  cols_2l = c("#1F78B4", "#33A02C")

  # get x,y,name of left points
  tbl_singles1 <- tbl_singles %>%
    arrange(desc(mean)) %>%
    mutate(x1 = inset_x, y1 = -1 + inset_y + nrow(.):1) %>% 
    select(-mean) 
  
  # add to pairs table pv levels, pv simbols, connect to two single locations 
  suppressWarnings(
    tbl_points <- tbl_pairs %>%
      #     mutate(p_levels=cut(p_value,breaks = c(0,0.001,0.01,0.05,0.1,1),p_levels),
      #            p_simbols=cut(p_value,breaks = c(0,0.001,0.01,0.05,0.1,1),labels = c("***","**","*",".","")),
      #            p_simbols_adj=cut(p_value,breaks = c(0,0.001,0.01,0.05,0.1,1),labels = c("***","**","*",".",""))) %>% 
      inner_join(tbl_singles1,c("name1" = "name")) %>% 
      inner_join(tbl_singles1,c("name2" = "name")) %>% 
      mutate(y=(y1.x+y1.y)/2,x=inset_x+abs(y1.x-y1.y)/sqrt(2),pair_id=row_number())
  )
    
  tbl_seg1 <- tbl_points %>% select(name1,name2,x1 = x1.x,y1 = y1.x,x2 = x, y2 = y,pair_id)
  tbl_seg2 <- tbl_points %>% select(name1,name2,x1 = x1.y,y1 = y1.y,x2 = x, y2 = y,pair_id)
  tbl_seg <- bind_rows(tbl_seg1,tbl_seg2) %>% arrange(desc(x2)) %>% mutate(line = 1:nrow(.))
  tbl_seg_begin <- tbl_seg %>% transmute(name1,name2, x = x1, y = y1, line,pos = "begin",pair_id)
  tbl_seg_end <- tbl_seg %>% transmute(name1,name2, x = x2, y = y2, line, pos = "end",pair_id)
  tbl_seg_long <- bind_rows(tbl_seg_begin,tbl_seg_end) %>% group_by(line)
  
  ggv <- ggvis() %>%
    add_data(tbl_seg_long) %>%
    scale_numeric("x", domain = c(0, max(tbl_seg_long$x)+0.5*inset_x)) %>% 
    scale_numeric("y", domain = c(0, max(tbl_seg_long$y)+0.5*inset_y)) %>% 
    #scale_ordinal("stroke",domain = p_levels,reverse = T,range = cols) %>% 
    layer_paths(x = ~x , y = ~y,strokeWidth := 4,stroke := cols[2]) %>%  # =~p_levels) %>% 
    add_data(tbl_points) %>%
    layer_points(x = ~x , y = ~y, size := 40 , fill :=cols[4] ,key := ~pair_id) %>% 
    layer_points(x = ~x , y = ~y, size := 140, fillOpacity := 0,stroke := cols_2l[1],strokeWidth := 2,key := ~pair_id, data = tbl_points %>% filter(p_value<=alpha)) %>% 
    layer_points(x = ~x , y = ~y, size := 280, fillOpacity := 0,stroke := cols_2l[2],strokeWidth := 2,key := ~pair_id, data = tbl_points %>% filter(p_value_adj<=alpha)) %>% 
    hide_axis("x") %>% hide_axis("y") %>% 
    layer_text(x=~x1,y=~y1,text:=~name,data = tbl_singles1,
               align:="right",baseline:="middle",dx:=-10,fontSize:=16) %>% 
    scale_ordinal("stroke",domain = c("GxL Adjusted","unadjusted"),range = rev(cols_2l)) %>% 
    add_legend(title = paste0("significant (alpha = ",alpha,")"),scales = c("stroke","size"),
               properties = legend_props(symbols = list(strokeWidth = 2,size = 140),
                                         labels = list(fontSize=13),
                                         title = list(fontSize=15,fontWeight ="normal"),
                                         legend = NULL))
    
  ggv %>% add_tooltip(tooltip_pv,c("hover"))
}
      
#     
#     
#     
#   on_mouse_over <- function(data,location,session)
#     print(ls())
#   {
#     # over segment:
#     if(!is.null(data$line))
#     {
#       tbl_selected_pair_id <- tbl_seg_long %>% filter(line == data$line) %>% select(pair_id)
#       selected <- unique(tbl_selected_pair_id$pair_id) # selected <- 1
#       rows <- tbl_seg_long %>% filter(pair_id == selected)
#       layer_paths(vis,data = rows,x = ~x , y = ~y,strokeWidth := 4,stroke := "blue")
#       layer_points(vis,data = tbl_points %>% filter(pair_id==selected),
#                    x = ~x , y = ~y, size := 45 , fill := "blue")
#       cat()
#     }
#     else # over point:
#     {
#       rows <- tbl_seg_long %>% filter(pair_id == data$pair_id)
#       layer_paths(vis,data = rows,x = ~x , y = ~y,strokeWidth := 4,stroke := "blue")
#       layer_points(vis,x = data$x , y = data$y, size := 45 , fill := "blue") 
#     }
#   }


get_datatable <- function(tbl_res,meassure)
{
  ## table sketch
  sketch <- withTags(table(
    class = 'display cell-border nowrap compact',
    thead(
      tr(
        th(rowspan = 2, 'Pair'),
        th(rowspan = 2, HTML('Unadjusted<br> p-value')),
        th(rowspan = 2, HTML('GxL adjusted<br> p-value')),
        th(rowspan = 2, 'Difference'),
        th(colspan = 2, 'Unadjusted CI'),
        th(colspan = 2, 'GxL adjusted CI')
      ),
      tr(
        lapply(rep(c('lwr', 'upr'), 2), th)
      )
    )
  ))
  
  ## table caption:
  if (is.null(meassure))
    caption = NULL
  else
  {
    caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;padding;0;',
      em(paste("Measure:",meassure))
    )
  }
  
  ## format table:
  if (!all(is.na(tbl_res)))
    tbl_res <- as.data.frame(cbind(
      format(tbl_res[,1:2],digits = 2,nsmall = 3,scientific = 4),
      format(tbl_res[,3:7],digits = 3,nsmall = 3,scientific = 4)
    ))
  
  ## create DT object
  datatable(tbl_res,caption = caption,
            selection = "none", container = sketch,escape = T,
            options = list(paging =  F, dom='t', ordering = F, processing = T))
}


# # calculate siginificant digits for table (like display in xtable) [deprecated]
# sig_digit_est <- 
#   c(tbl_res[,5]-tbl_res[,4],tbl_res[,3:5]) %>% # take the CIs lengths and all estimates values
#   abs() %>% min() %>%                                      # the the minimum of thier absolute values 
#   log(10) %>% {3-floor(.)} %>%                             # get its approximate order of magnitude, plus 3 is the desired digits to display
#   max(3)                                                   # or 3 (the maximum of both)
# sig_digit_pv <- 
#   tbl_res[,1] %>% min() %>%                            # take the minimum pv  abs() %>% min() %>% # the the minimum of thier absolute values 
#   log(10) %>% {2-floor(.)} %>%                             # get its approximate order of magnitude, plus 3 is the desired digits to display
#   max(4)                                                   # or 4 (the maximum of both)


