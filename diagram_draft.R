# save(o,file = "o.RData")
load("o.RData")
library(ggvis)
library(dplyr)

tbl_res <- o$tbl_res[,1:2]
pair_names <- matrix(unlist((strsplit(rownames(tbl_res)," - "))),ncol=2,byrow = T)
tbl_pairs <- data.frame(name1 = pair_names[,1],name2 = pair_names[,2],tbl_res)
tbl_singles <- data.frame(name=o$genotypes_tested,mean=o$stats_mat[,1])

# op <- options()
# options(scipen = 3)
# options(digits = 5)
# plot_diagram <- function(tbl_singles,tbl_pairs)
# {
  alpha <- 0.05
  inset_x <- 2
  inset_y <- 1
  
  cols <- c("#BDBDBD","#969696","#737373","#525252","#252525") #brewer.pal(n=9,"Greys")[4:8] 
  cols_2l = c("#1F78B4", "#33A02C")
# p_levels <- levels(cut(0,breaks = c(0,0.001,0.01,0.05,0.1,1)))
  
  # get x,y,name of left points
  tbl_singles1 <- tbl_singles %>%
    arrange(desc(mean)) %>%
    mutate(x1 = inset_x, y1 = -1 + inset_y + nrow(.):1) %>% 
    select(-mean) 
  
  # add to pairs table pv levels, pv simbols, connect to two single locations 
  tbl_points <- tbl_pairs %>%
#     mutate(p_levels=cut(p_value,breaks = c(0,0.001,0.01,0.05,0.1,1),p_levels),
#            p_simbols=cut(p_value,breaks = c(0,0.001,0.01,0.05,0.1,1),labels = c("***","**","*",".","")),
#            p_simbols_adj=cut(p_value,breaks = c(0,0.001,0.01,0.05,0.1,1),labels = c("***","**","*",".",""))) %>% 
    inner_join(tbl_singles1,c("name1" = "name")) %>% 
    inner_join(tbl_singles1,c("name2" = "name")) %>% 
    mutate(y=(y1.x+y1.y)/2,x=inset_x+abs(y1.x-y1.y)/sqrt(2),pair_id=row_number())
    
  
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
    
    
    ggv %>% add_tooltip(tooltip_pv,c("hover","click"))
    
    
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
             <td class="tg-a3u1">•</td>
             <td class="tg-vkoad">
             ',format(row$p_value_adj,digits = 4),'</td>
             </tr>
             <tr>
             <td class="tg-wja7">•</td>
             <td class="tg-vkoua">
             ',format(row$p_value,digits = 4),'</td>
             </tr>
             </table>'
      )
             }
    
    
  on_mouse_over <- function(data,location,session)
    print(ls())
  {
    # over segment:
    if(!is.null(data$line))
    {
      tbl_selected_pair_id <- tbl_seg_long %>% filter(line == data$line) %>% select(pair_id)
      selected <- unique(tbl_selected_pair_id$pair_id) # selected <- 1
      rows <- tbl_seg_long %>% filter(pair_id == selected)
      layer_paths(vis,data = rows,x = ~x , y = ~y,strokeWidth := 4,stroke := "blue")
      layer_points(vis,data = tbl_points %>% filter(pair_id==selected),
                   x = ~x , y = ~y, size := 45 , fill := "blue")
      cat()
    }
    else # over point:
    {
      rows <- tbl_seg_long %>% filter(pair_id == data$pair_id)
      layer_paths(vis,data = rows,x = ~x , y = ~y,strokeWidth := 4,stroke := "blue")
      layer_points(vis,x = data$x , y = data$y, size := 45 , fill := "blue") 
    }
  }
  