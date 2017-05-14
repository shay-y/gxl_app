library(shiny)
library(tidyverse)
library(stringr)
library(shinyBS)
library(shinyjs)
library(DT)
library(rdrop2)

options(
  shiny.maxRequestSize = 10e6
  # ## for development and debugging:
  # shiny.testmode = TRUE,
  # shiny.reactlog=TRUE,
  # shiny.minified = FALSE,
  # shiny.sanitize.errors = TRUE,
  # dplyr.print_max = 50,
  # dplyr.width = Inf,
  # width = 1000
)

source("pcci.R")

tbl_metadata   <- read_csv("data/tbl_metadata.csv"  )
tbl_models     <- read_csv("data/tbl_models.csv"    )
tbl_procedures <- read_csv("data/tbl_procedures.csv")
tbl_example_raw_data <- read_csv("data/tbl_example_mrc_oft_pas.csv")
example_group_names_vec <- readRDS("data/example_group_names_vec.rds")
procedure_name_list <-  tbl_procedures %>% .$procedure_name %>% as.character() %>% as.list() %>% {setNames(.,.)}
group_names_list <- example_group_names_vec %>%
  {setNames(.,.)} %>% 
  list("Select group names from the list or create new names" = .)
  
drop_dir <- "GxL/gxl_app_userdata_files/"
token    <- readRDS("droptoken.rds")