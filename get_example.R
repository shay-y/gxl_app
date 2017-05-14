library(tidyverse)
Males <- read_csv(file = "../../Dropbox/GxL/Addressing reproducibility... - Supplementary Materials/input_datasets/IMPC_Males.csv",col_types = cols(lab = col_character(), strain = col_character(),.default = "d"))
Females <- read_csv(file = "../../Dropbox/GxL/Addressing reproducibility... - Supplementary Materials/input_datasets/IMPC_Females.csv",col_types = cols(lab = col_character(), strain = col_character(),.default = "d"))

tbl_examples <- 
  bind_rows(Males = Males,Females = Females,.id = "sex")  %>%
  gather(key = parameter, value = value,-sex,-lab,-strain,na.rm = T) %>%
  mutate(eslim_id = parameter %>% str_split_fixed(pattern = " ",n = 2) %>% .[,1]) %>% 
  filter(!is.na(value),strain %in% c("Arhgef4","baseline","Elk4","Setmar","Slc38a10","Tnfaip1","Ttll4")) %>% 
  inner_join(tbl_models,by = c("eslim_id","sex"))
  
#tbl_models %>% select(strains) %>% distinct()

mult_adjust <- "BH selected"
alpha=0.05 
xx <- tbl_examples %>% 
  group_by(sex,lab,procedure_name,parameter_name,strain) %>% 
  mutate(transformed = eval(parse(text=paste("function(x)",transformation_expr)))(value)) %>% 
  summarise(mean.t = mean(transformed,na.rm = T),
            #mean = mean(value,na.rm = T),
            sd.t =sd(transformed,na.rm = T),
            #sd =sd(value,na.rm = T),
            nn =sum(!is.na(transformed)),
            s2_ratio = unique(s2_ratio),
            n_labs_s2gxl = unique(n_labs_s2gxl),
            n_groups_s2gxl = unique(n_groups_s2gxl)) %>%
  group_by(sex,procedure_name,parameter_name,lab)
  

xx1 <- xx %>% 
  do(
    {
      s2_ratio = unique(.$s2_ratio)
      n_labs_s2gxl = unique(.$n_labs_s2gxl)
      n_groups_s2gxl = unique(.$n_groups_s2gxl)
      tbl_summ <-  tibble(group_name=.$strain,mean.t = .$mean.t, sd.t = .$sd.t,n = .$nn) %>% rownames_to_column(var = "group_id")
      tbl_summ <- tbl_summ %>%
        mutate(
          df_pooled = sum(n)-n(),
          s2_pooled = sum(sd.t^2*(n-1))/df_pooled
        )
      nmeans_tukey <- nrow(tbl_summ)
      tbl_pairs_ <- nmeans_tukey %>% combn(2) %>% t() %>% .[,2:1] %>% as_tibble() 
      tbl_pairs_ <- tbl_pairs_ %>% 
        transmute(group_id1 = as.character(V1),group_id2 = as.character(V2)) %>% 
        inner_join(tbl_summ,by = c("group_id1"="group_id")) %>%
        inner_join(tbl_summ,c("group_id2" = "group_id", "s2_pooled", "df_pooled")) %>% 
        transmute(
          pair_id = paste0(group_id1,"-",group_id2),
          name_pair = paste(group_name.x,"-",group_name.y),
          grp1  = group_name.x,
          grp2  = group_name.y,
          mean.t.x,
          mean.t.y,
          sd.t.x,
          sd.t.y,
          n.x,
          n.y,
          s2_pooled,
          df_pooled,
          
          diff  = mean.t.x-mean.t.y,                  
          se    = sqrt(s2_pooled * (1/n.x+1/n.y) ),
          se_gxl= sqrt(s2_pooled * (1/n.x+1/n.y+2*s2_ratio) ),
          stat     = abs(diff) / se,
          stat_gxl = abs(diff) / se_gxl,
          
          df_gxl = ( (1/n.x+1/n.y) * s2_pooled + 2 * s2_ratio * s2_pooled ) ^2   /
            ( ((1/n.x+1/n.y) * s2_pooled)^2/df_pooled + (2 * s2_ratio * s2_pooled)^2 / (n_labs_s2gxl-1) / (n_groups_s2gxl-1) ),
          
          pv      = 2*(1-pt(q = stat    ,df = df_pooled )),
          pv_gxl  = 2*(1-pt(q = stat_gxl,df = df_gxl    )))
      
      if (mult_adjust == "Tukey HSD")
        
        tbl_pairs_ <- tbl_pairs_ %>%  mutate(
          pv      = ptukey(q = sqrt(2) * stat    , nmeans = nmeans_tukey, df = df_pooled, lower.tail = F),
          pv_gxl  = ptukey(q = sqrt(2) * stat_gxl, nmeans = nmeans_tukey, df = df_gxl   , lower.tail = F),
          
          lwr     = diff - qtukey(p = 1-alpha, nmeans = nmeans_tukey, df = df_pooled) * se / sqrt(2),
          upr     = diff + qtukey(p = 1-alpha, nmeans = nmeans_tukey, df = df_pooled) * se / sqrt(2),
          
          lwr_gxl = diff - qtukey(p = 1-alpha, nmeans = nmeans_tukey, df = df_gxl)*se_gxl / sqrt(2),
          upr_gxl = diff + qtukey(p = 1-alpha, nmeans = nmeans_tukey, df = df_gxl)*se_gxl / sqrt(2))
      
      if (mult_adjust == "BH selected")
        
        tbl_pairs_ <- tbl_pairs_ %>%  mutate(
          pv     = p.adjust(pv    ,"BH"),
          pv_gxl = p.adjust(pv_gxl,"BH"),
          
          Q     = max(1, sum(pv     <= alpha)) / n(),
          Q_gxl = max(1, sum(pv_gxl <= alpha)) / n(),
          
          lwr = diff - qt(1-alpha/2*Q ,df = df_pooled)*se %>% as.numeric(),
          upr = diff + qt(1-alpha/2*Q ,df = df_pooled)*se %>% as.numeric(),
          
          lwr_gxl = diff - qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric(),
          upr_gxl = diff + qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric())
      
      if (mult_adjust == "none")
        
        tbl_pairs_ <- tbl_pairs_ %>% mutate(
          lwr = diff - qt(1-alpha/2 ,df = df_pooled)*se %>% as.numeric(),
          upr = diff + qt(1-alpha/2 ,df = df_pooled)*se %>% as.numeric(),
          
          lwr_gxl = diff - qt(1-alpha/2 ,df = df_gxl)*se_gxl %>% as.numeric(),
          upr_gxl = diff + qt(1-alpha/2 ,df = df_gxl)*se_gxl %>% as.numeric())
      
      tbl_pairs_
      
    }
  )
         
xx1 %>% View() # and pick one


#  ------------------------------------------------------------------------

tbl_example <- 
  Males %>% 
  filter(lab == "MRC_Harwell") %>% 
  select(strain,`ESLIM_007_001_009 Open-field Periphery average speed`) %>% 
  filter(!is.na(`ESLIM_007_001_009 Open-field Periphery average speed`),
         strain %in% c("baseline","Setmar","Slc38a10","Tnfaip1")) %>%  # "Arhgef4" "Elk4" "Ttll4"
  mutate(strain = ifelse(strain=="baseline","C57BL/6J",strain))
tbl_example %>% 
  write_csv(path = "./data/tbl_example_mrc_oft_pas.csv",col_names = F)




