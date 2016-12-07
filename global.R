# install.packages(c("tidyverse","shiny","shinyjs","shinyBS","DT","rdrop2"))

library(tidyverse,warn.conflicts = F)
library(stringr)
library(shiny)
library(shinyBS)
library(shinyjs,warn.conflicts = F)
library(DT,warn.conflicts = F)
library(rdrop2)

options(
  #shiny.testmode = TRUE,
  # shiny.reactlog=TRUE,
  #shiny.autoreload = TRUE,
  #shiny.autoreload.pattern = glob2rx(c("ui.R","server.R","style.css","WWW/style.css")),
  #shiny.autoreload.interval = 2000
  # shiny.maxRequestSize = 10e6,
  # shiny.minified = FALSE,
  # shiny.error = browser,
  # shiny.table.class = "gxl-table",
  # shiny.sanitize.errors = TURE,
  # dplyr.print_max = 50,
  # dplyr.width = Inf,
  # width = 1000,
  # DT.options = list(dom = 'tB')
)

source("pcci.R")

tbl_metadata   <- read_csv("data/tbl_metadata.csv"  )
tbl_models     <- read_csv("data/tbl_models.csv"    )
tbl_procedures <- read_csv("data/tbl_procedures.csv")
tbl_example_raw_data <- read_csv("data/tbl_example.csv"   )
example_group_names_vec <- readRDS("data/example_group_names_vec.rds")
procedure_name_list <-  tbl_procedures %>% .$procedure_name %>% as.character() %>% as.list() %>% {setNames(.,.)}
group_names_list <- example_group_names_vec %>%
  {setNames(.,.)} %>% 
  list("Select group names from the list or create new names" = .)
  
drop_dir <- "GxL/gxl_app_userdata_files/"
token    <- readRDS("droptoken.rds")











