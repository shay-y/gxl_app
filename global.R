# options(shiny.reactlog=TRUE)
# install.packages(c("devtools","ggplot2","dplyr","tidyr","shiny","shinyjs","DT","shinyBS"))
# setwd("~/Dropbox/App_GxL/gxl_app")

# install.packages("RColorBrewer")
# library(RColorBrewer)
# (cols <- brewer.pal(4,"Paired")[c(2,4)])
# library(ggrepel,warn.conflicts = F)
# library(coefplot)

library(ggplot2)
library(shiny)
library(shinyBS)
library(DT,warn.conflicts = F)
library(dplyr,warn.conflicts = F)
library(shinyjs,warn.conflicts = F)
library(stringr)
library(tidyr)
library(tibble)
source("pcci.R")

options(dplyr.width = Inf,dplyr.print_max = 100,width = 1000)
lab_names_vec <- NULL
load("Startup_objects.RData")

tbl_example_measure_input <- read.csv("data/Simplified IPGTT Glucose response AUC - example.csv",header = F)

# procedure_name_list <-  tbl_procedures_uploaded %>% .$procedure_name %>% as.character() %>% as.list() %>% {setNames(.,.)}
# genotype_list <-
#   c("129S1/SvImJ","A/J","AKR/J","BALB/cByJ","BTBR_T/1_tf/tf","C3H/HeJ","C57BL/6J","C57L/J","C58/J","CAST/Ei","DBA/2J","FVB/NJ","MOLF/Ei","NOD/LtJ","NZB/B1NJ","PERA/Ei","PL/J","SJL/J","SM/J","SPRET/Ei","SWR/J","C57BL/6N","DBA/2","g1","g2","g3","g4","gctrl","Arhgef4","baseline","Elk4","Setmar","Slc38a10","Tnfaip1","Ttll4") %>%
#   as.character() %>%
#   as.list() %>%
#   {setNames(.,.)}



# tbl_example_summaries <- 
#   tbl_example_measure_input %>% 
#   mutate(transformed = round(V2)) %>% 
#   group_by(V1) %>% 
#   summarise(mean_t = mean(transformed,na.rm = T),
#             mean = mean(V2,na.rm = T),
#             sd_t =sd(transformed,na.rm = T),
#             sd =sd(V2,na.rm = T),
#             n =sum(!is.na(transformed)))

## 

library(rdrop2)
log_file_drop_dir <- "app_gxl/gxl_app_logs/"
#drop_get(path = paste0(log_file_drop_dir,log_file_name),overwrite = T)


options(DT.options = list(dom = 't'))

