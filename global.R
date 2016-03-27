# options(shiny.reactlog=TRUE)
# install.packages(c("devtools","multcomp","ggplot2"))
# library(devtools)
# devtools::install_github(c(
#   "hadley/dplyr",
#   "hadley/tidyr",
#   "rstudio/shiny",
#   "daattali/shinyjs",
#   "rstudio/DT"))


#rm(list = ls())
#library(multcomp)
#library(tidyr)
library(shiny)
library(dplyr)
library(shinyjs)
library(DT)

lab_names_vec <- NULL
load("Startup_objects.RData")


# suppressWarnings({
#   tbl_IMPC <- read.csv(file = "tbl_IMPC.csv",stringsAsFactors = F,encoding = "UTF-8") %>% tbl_df()
#   
#   ## temporary hard coded entries of latest versions of selected procedures
#   latest_procedure_stable_ids <- 
#     c("IMPC_CSD_003","IMPC_OFD_001","IMPC_GRS_001",
#       "IMPC_ACS_003","IMPC_IPG_001","IMPC_DXA_001","IMPC_CBC_003") # "IMPC_HOU_001","IMPC_EXD_001","IMPC_BWT_001"
#   
#   ## to implement later:
#   # tbl_IMPC %>%
#   #   select(procedure_stable_id,major_version.x,minor_version.x) %>% 
#   #   mutate(procedure_stable_id_trunc = substr(procedure_stable_id,start = 1,stop = 8)) %>% 
#   #   distinct() %>%
#   #   group_by(procedure_stable_id_trunc) %>% ...
#   
#   tbl_metadata <- 
#     tbl_IMPC %>% 
#     filter(
#       parameter_type == "procedureMetadata" &
#         required &
#         procedure_stable_id %in% latest_procedure_stable_ids &
#         !(parameter_name %in% c("Equipment ID","Experimenter ID","Blood collection experimenter ID","Blood analysis experimenter ID"))
#     ) %>% 
#     select(procedure_stable_id,procedure_name,stage_label,parameter_stable_id,parameter_name,description.x,unit,datatype,data_analysis,option_name)
#   
#   tbl_sex_temp <- tbl_metadata %>% 
#     transmute(procedure_stable_id,procedure_name,parameter_name = "Sex",datatype = "TEXT",data_analysis = 1) %>%
#     distinct() %>% 
#     merge(data_frame(option_name =c("Both","Males","Females"))) %>% 
#     tbl_df()
#   
#   tbl_stage_temp <- tbl_metadata %>%
#     transmute(.,procedure_stable_id,procedure_name,parameter_name = "Stage",option_name = stage_label,datatype = "TEXT",data_analysis = 1) %>%
#     distinct()
#   
#   tbl_metadata_1 <- tbl_sex_temp %>% 
#     full_join(tbl_stage_temp) %>%
#     full_join(tbl_metadata) %>% 
#     group_by(procedure_stable_id,procedure_name,stage_label,parameter_stable_id,parameter_name,description.x,unit,datatype,data_analysis) %>% 
#     summarize(options = {option_name %>% as.character() %>% list()}) %>% 
#     replace(is.na(.), "") %>% 
#     group_by(procedure_stable_id) %>% 
#     arrange(parameter_stable_id)
#   
#   tbl_procedures <- tbl_IMPC %>% 
#     select(procedure_name,procedure_stable_id) %>% 
#     filter(procedure_stable_id %in% latest_procedure_stable_ids) %>% 
#     distinct()
#   
#   tbl_procedures_1 <- tbl_procedures %>% 
#     left_join(
#       data_frame(
#         procedure_stable_id = 
#           c("IMPC_BWT_001",	
#             "IMPC_HOU_001",	
#             "IMPC_EXD_001",	
#             "IMPC_CSD_003",	
#             "IMPC_OFD_001",	
#             "IMPC_GRS_001",	
#             "IMPC_ACS_003",	
#             "IMPC_IPG_001",	
#             "IMPC_DXA_001",	
#             "IMPC_CBC_003"),
#         url = 
#           c("https://www.mousephenotype.org/impress/protocol/103/7",
#             "https://www.mousephenotype.org/impress/protocol/173/14",
#             "https://www.mousephenotype.org/impress/protocol/188/7",
#             "https://www.mousephenotype.org/impress/protocol/186/7",
#             "https://www.mousephenotype.org/impress/protocol/81/7",
#             "https://www.mousephenotype.org/impress/protocol/83/7",
#             "https://www.mousephenotype.org/impress/protocol/176/7",
#             "https://www.mousephenotype.org/impress/protocol/87/7",
#             "https://www.mousephenotype.org/impress/protocol/90/7",
#             "https://www.mousephenotype.org/impress/protocol/182/7")
#       ))
#   
#   procedure_name_list <-  tbl_procedures_1 %>% .$procedure_name %>% as.character() %>% as.list() %>% {setNames(.,.)}
#   
#   ## for debuging 
#   # tbl_metadata_selected <- tbl_metadata_1 %>% 
#   #   filter(procedure_name == "Open Field")
#   
#   tbl_measure <-
#     tbl_IMPC %>% 
#     filter(parameter_type == "simpleParameter" & procedure_stable_id %in% latest_procedure_stable_ids,datatype != "TEXT") %>% 
#     select(procedure_stable_id,procedure_name,stage_label,parameter_stable_id,parameter_name,description.x,unit,datatype,data_analysis,option_name)
#   
#   tbl_measure_csv <- read.csv(file = "tbl_measures_v3.csv",stringsAsFactors = F,encoding = "UTF-8") %>% tbl_df()
#   
#   tbl_measure_1 <- 
#     tbl_measure_csv %>% select(-procedure_name,-parameter_name) %>% 
#     inner_join(tbl_measure,"parameter_stable_id") %>% 
#     select(-stage_label,-description.x,-data_analysis,-option_name) %>% 
#     mutate(s2_ratio = s2_interaction/s2_error) %>% 
#     filter(sex == "male")
#   
#   # humanDate <- function() format(Sys.time(), "%Y_%m_%d")
#   
#   # procedure_desc
#   genotype_list <- 
#     c("129S1/SvImJ","A/J","AKR/J","BALB/cByJ","BTBR_T/1_tf/tf","C3H/HeJ","C57BL/6J","C57L/J","C58/J","CAST/Ei","DBA/2J","FVB/NJ","MOLF/Ei","NOD/LtJ","NZB/B1NJ","PERA/Ei","PL/J","SJL/J","SM/J","SPRET/Ei","SWR/J","C57BL/6N","DBA/2","g1","g2","g3","g4","gctrl","Arhgef4","baseline","Elk4","Setmar")#,"Slc38a10","Tnfaip1","Ttll4") %>% 
#   as.character() %>%
#     as.list() %>%
#     {setNames(.,.)}
# })
# save.image(file = "Startup_objects.RData")

tbl_example_measure_input <- read.csv("data/Simplified IPGTT Glucose response AUC - example.csv",header = F)
tbl_example_summaries <- 
  tbl_example_measure_input %>% 
  mutate(transformed = round(V2)) %>% 
  group_by(V1) %>% 
  summarise(mean_t = mean(transformed,na.rm = T),
            mean = mean(V2,na.rm = T),
            sd_t =sd(transformed,na.rm = T),
            sd =sd(V2,na.rm = T),
            n =sum(!is.na(transformed)))

