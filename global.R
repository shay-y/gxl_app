# options(shiny.reactlog=TRUE)
# install.packages(c("devtools","ggplot2","dplyr","tidyr","shiny","shinyjs","DT","shinyBS"))

library(tidyverse)
library(stringr)
library(shiny)
library(shinyBS)
library(shinyjs,warn.conflicts = F)
library(DT,warn.conflicts = F)
library(rdrop2)

source("pcci.R")

load("Startup_objects.RData")

options(
  # dplyr.width = Inf,
  dplyr.print_max = 50,
  width = 1000,
  DT.options = list(dom = 't')
  )

tbl_example_measure_input <- read.csv("data/Simplified IPGTT Glucose response AUC - example.csv",header = F)
tbl_exmp_names <- tbl_examples %>% select(exmp_name) %>% distinct()
procedure_name_list <-  tbl_procedures %>% .$procedure_name %>% as.character() %>% as.list() %>% {setNames(.,.)}
group_names_list <- example_group_names_vec %>%
  {setNames(.,.)} %>% 
  list("Select group names from the list or create new names" = .)
  
drop_dir <- "GxL/gxl_app_userdata_files/"
token <- readRDS("droptoken.rds")

#drop_get(path = paste0(log_file_drop_dir,log_file_name),overwrite = T)










