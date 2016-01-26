# options(shiny.reactlog=TRUE)

# install.packages(c("devtools","multcomp","ggplot2"))
# library(devtools)
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
# library(shinyIncubator)
library(shinyAce)
library(shinyjs)
library(DT)
library(ggvis)

# library(devtools)
# devtools::install_github("hadley/readr")
# devtools::install_github("jennybc/googlesheets")

## prepare the OAuth token and set up the target sheet:
##  - do this interactively
##  - do this EXACTLY ONCE

# library(googlesheets)
# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "shiny_app_token.rds")
# ss <- gs_new("10_read-write-private-sheet",
#              row_extent = n, col_extent = n, input = filler)
# ss$sheet_key # 10kYZGTfXquVUwvBXH-8M-p01csXN6MNuuTzxnDdy3Pk

## if you version control your app, don't forget to ignore the token file!
## e.g., put it into .gitignore

# googlesheets::gs_auth(token = "shiny_app_token.rds")
# sheet_key <- "10kYZGTfXquVUwvBXH-8M-p01csXN6MNuuTzxnDdy3Pk"
# ss <- googlesheets::gs_key(sheet_key)

plot_h <- 1
## read datasets:
tbl_measures <- read.csv("data/app_measures_tbl_v1.csv") %>% tbl_df()
tbl_meta <- read.csv("data/app_meta_tbl_v1.csv") %>% tbl_df()
genotypes_vec <- c("129S1/SvImJ","A/J","AKR/J","BALB/cByJ","BTBR_T/1_tf/tf","C3H/HeJ","C57BL/6J","C57BL/6N","C57L/J","C58/J","CAST/Ei","DBA/2J","FVB/NJ","MOLF/Ei","NOD/LtJ","NZB/B1NJ","PERA/Ei","PL/J","SJL/J","SM/J","SPRET/Ei","SWR/J","C57BL/6N","DBA/2","g1","g2","g3","g4","gctrl")
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


