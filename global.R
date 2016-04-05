# options(shiny.reactlog=TRUE)
# install.packages(c("devtools","ggplot2"))
# library(devtools)
# devtools::install_github(c(
#   "hadley/dplyr",
#   "hadley/tidyr",
#   "rstudio/shiny",
#   "daattali/shinyjs",
#   ))
# setwd("~/Dropbox/App_GxL/gxl_app")

# install.packages("RColorBrewer")
# library(RColorBrewer)
# (cols <- brewer.pal(4,"Paired")[c(2,4)])

library(ggplot2)
library(shiny)
library(dplyr,warn.conflicts = F)
library(shinyjs)
library(stringr)
library(tidyr)
options(dplyr.width = Inf,dplyr.print_max = 100,width = 1000)
lab_names_vec <- NULL
load("Startup_objects.RData")

tbl_example_measure_input <- read.csv("data/Simplified IPGTT Glucose response AUC - example.csv",header = F)
# tbl_example_summaries <- 
#   tbl_example_measure_input %>% 
#   mutate(transformed = round(V2)) %>% 
#   group_by(V1) %>% 
#   summarise(mean_t = mean(transformed,na.rm = T),
#             mean = mean(V2,na.rm = T),
#             sd_t =sd(transformed,na.rm = T),
#             sd =sd(V2,na.rm = T),
#             n =sum(!is.na(transformed)))

