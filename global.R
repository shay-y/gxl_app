# options(shiny.reactlog=TRUE)
# install.packages(c("devtools","tidyverse","shiny","shinyjs","DT","shinyBS"))

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

tbl_example_raw_data <- read.csv("data/MRC_Harwell_Males_Fat_Body weight.csv", header = F)
tbl_exmp_names <- tbl_examples %>% select(exmp_name) %>% distinct()
procedure_name_list <-  tbl_procedures %>% .$procedure_name %>% as.character() %>% as.list() %>% {setNames(.,.)}
group_names_list <- example_group_names_vec %>%
  {setNames(.,.)} %>% 
  list("Select group names from the list or create new names" = .)
  
drop_dir <- "GxL/gxl_app_userdata_files/"
token    <- readRDS("droptoken.rds")











