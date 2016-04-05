#install.packages('gsheet')
library(dplyr,warn.conflicts = F)
library(readr)
op <- options()
options(warn = -1)
library(stringr)
library(tidyr)
library(gsheet)

setwd("~/Dropbox/App_GxL/preproc")
## tbl_IMPC is a result query for all parameters starting IMPC_ from MySQL db see file starting with "get_IMPC_..
tbl_IMPC <- read.csv(file = "tbl_IMPC.csv",stringsAsFactors = F,encoding = "UTF-8") %>% tbl_df()

## temporary hard coded entries of latest versions of selected procedures
latest_procedure_stable_ids <-
  c("IMPC_CSD_003","IMPC_OFD_001","IMPC_GRS_001",
    "IMPC_ACS_003","IMPC_IPG_001","IMPC_DXA_001","IMPC_CBC_003") # "IMPC_HOU_001","IMPC_EXD_001","IMPC_BWT_001"

tbl_metadata <-
  tbl_IMPC %>%
  filter(
    parameter_type == "procedureMetadata" &
      required &
      procedure_stable_id %in% latest_procedure_stable_ids &
      !(parameter_name %in% c("Equipment ID","Experimenter ID","Blood collection experimenter ID","Blood analysis experimenter ID"))
  ) %>%
  select(procedure_stable_id,procedure_name,stage_label,parameter_stable_id,parameter_name,description.x,unit,datatype,data_analysis,option_name)

tbl_sex_temp <- tbl_metadata %>%
  transmute(procedure_stable_id,procedure_name,parameter_name = "Sex",datatype = "TEXT",data_analysis = 1) %>%
  distinct() %>%
  merge(data_frame(option_name =c("Both","Males","Females"))) %>%
  tbl_df()

tbl_stage_temp <- tbl_metadata %>%
  transmute(.,procedure_stable_id,procedure_name,parameter_name = "Stage",option_name = stage_label,datatype = "TEXT",data_analysis = 1) %>%
  distinct()

tbl_metadata_1 <- tbl_sex_temp %>%
  full_join(tbl_stage_temp) %>%
  full_join(tbl_metadata) %>%
  group_by(procedure_stable_id,procedure_name,stage_label,parameter_stable_id,parameter_name,description.x,unit,datatype,data_analysis) %>%
  summarize(options = {option_name %>% as.character() %>% list()}) %>%
  replace(is.na(.), "") %>%
  group_by(procedure_stable_id) %>%
  arrange(parameter_stable_id)

tbl_procedures <- tbl_IMPC %>%
  select(procedure_name,procedure_stable_id) %>%
  filter(procedure_stable_id %in% latest_procedure_stable_ids) %>%
  distinct()

tbl_procedures_1 <- tbl_procedures %>%
  left_join(
    data_frame(
      procedure_stable_id =
        c("IMPC_BWT_001",
          "IMPC_HOU_001",
          "IMPC_EXD_001",
          "IMPC_CSD_003",
          "IMPC_OFD_001",
          "IMPC_GRS_001",
          "IMPC_ACS_003",
          "IMPC_IPG_001",
          "IMPC_DXA_001",
          "IMPC_CBC_003"),
      url =
        c("https://www.mousephenotype.org/impress/protocol/103/7",
          "https://www.mousephenotype.org/impress/protocol/173/14",
          "https://www.mousephenotype.org/impress/protocol/188/7",
          "https://www.mousephenotype.org/impress/protocol/186/7",
          "https://www.mousephenotype.org/impress/protocol/81/7",
          "https://www.mousephenotype.org/impress/protocol/83/7",
          "https://www.mousephenotype.org/impress/protocol/176/7",
          "https://www.mousephenotype.org/impress/protocol/87/7",
          "https://www.mousephenotype.org/impress/protocol/90/7",
          "https://www.mousephenotype.org/impress/protocol/182/7")
    ))

procedure_name_list <-  tbl_procedures_1 %>% .$procedure_name %>% as.character() %>% as.list() %>% {setNames(.,.)}

tbl_measure <-
  tbl_IMPC %>%
  filter(parameter_type == "simpleParameter" & procedure_stable_id %in% latest_procedure_stable_ids,datatype != "TEXT") %>%
  select(procedure_stable_id,procedure_name,stage_label,parameter_stable_id,parameter_name,description.x,unit,datatype,data_analysis,option_name)

## download temp table with data from article about measures
url <- 'https://docs.google.com/spreadsheets/d/1_lswi7WvliET0uT1UdcVPhEXeNK2MfefQWYa2pv_7PE/edit#gid=594377777'
tbl_measure_gsheet <- gsheet2tbl(url)

tbl_measure_1 <-
  tbl_measure_gsheet %>% select(-procedure_name,-parameter_name) %>%
  inner_join(tbl_measure,"parameter_stable_id") %>%
  select(-stage_label,-description.x,-data_analysis,-option_name) %>%
  mutate(s2_ratio = s2_interaction/s2_error) %>%
  filter(sex == "male")

# procedure_desc
genotype_list <-
  c("129S1/SvImJ","A/J","AKR/J","BALB/cByJ","BTBR_T/1_tf/tf","C3H/HeJ","C57BL/6J","C57L/J","C58/J","CAST/Ei","DBA/2J","FVB/NJ","MOLF/Ei","NOD/LtJ","NZB/B1NJ","PERA/Ei","PL/J","SJL/J","SM/J","SPRET/Ei","SWR/J","C57BL/6N","DBA/2","g1","g2","g3","g4","gctrl","Arhgef4","baseline","Elk4","Setmar","Slc38a10","Tnfaip1","Ttll4") %>%
  as.character() %>%
  as.list() %>%
  {setNames(.,.)}

save(tbl_metadata_1,tbl_procedures_1,tbl_measure_1,procedure_name_list,genotype_list,file = "../gxl_app/Startup_objects.RData")
