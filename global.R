# devtools::install_github(c(
#   "hadley/dplyr",
#   "rstudio/shiny",
#   "rstudio/shiny-incubator",
#   "trestletech/shinyAce",
#   "daattali/shinyjs",
#   "rstudio/DT",
#   'rstudio/ggvis'))

library(multcomp)
library(dplyr)
library(shiny)
library(shinyIncubator)
library(shinyAce)
library(shinyjs)
library(DT)
library(ggvis)

## read datasets:
tbl_measures <- read.csv("data/app_measures_tbl_v1.csv") %>% tbl_df()
tbl_meta <- read.csv("data/app_meta_tbl_v1.csv") %>% tbl_df()
genotypes_vec <- c("129S1/SvImJ","A/J","AKR/J","BALB/cByJ","BTBR_T/1_tf/tf","C3H/HeJ","C57BL/6J","C57L/J","C58/J","CAST/Ei","DBA/2J","FVB/NJ","MOLF/Ei","NOD/LtJ","NZB/B1NJ","PERA/Ei","PL/J","SJL/J","SM/J","SPRET/Ei","SWR/J","C57BL/6N","DBA/2")
lab_names_vec <- NULL
measure_name_list <- NULL

## get table and list of procedures:
tbl_proc <- tbl_measures %>%
  select(Database,procedure_name) %>%
  distinct()
proc_IMPC_vec <- tbl_proc %>%
  filter(Database=="IMPC") %>%
  transmute(procedure_name) %>% 
  as.data.frame() %>%
  "["(,1) %>% 
  as.character()
proc_R2012_vec <- tbl_proc %>%
  filter(Database=="R2012") %>%
  transmute(procedure_name) %>% 
  as.data.frame() %>%
  "["(,1) %>% 
  as.character()
proc_name_list <- list(IMPC = proc_IMPC_vec, "Richer 2012" = proc_R2012_vec)

## functions:
source("functions.R")
source("plot_functions.R")
