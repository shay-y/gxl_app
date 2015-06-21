#devtools::install_github(c("rstudio/shiny-incubator","trestletech/shinyAce"))
library(shiny)
library(dplyr)
library(shinyIncubator)
library(shinyAce)
library(shinyjs)

humanDate <- function() format(Sys.time(), "%Y_%m_%d")

shinyServer(function(input, output, session) {
  
  ## when control is selected updated the choises for cases:
  observe({
    control_selected <- input$genotypes_tested_control
    updateSelectizeInput(session,inputId = "genotypes_tested_cases", choices = genotypes_vec[genotypes_vec!=control_selected], options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
  })
  
  ## when inputs in step 1 are changed, update the experiment identifier
  reactive({
    lab_name <- gsub('([[:punct:]])|\\s+','_',input$lab_name)
    if (input$expr_design=='Pairwise Comparisons')
    {
      expr_design <- "Pairwise"
      genotypes_tested <- input$genotypes_tested_pairwise
    }
      
    if (input$expr_design=='Control vs. Cases')
    {
      expr_design <- "CtrlCases"
      genotypes_tested <- c(input$genotypes_tested_control,input$genotypes_tested_cases)
    }
      
    genotypes_tested_init <- paste(substr(genotypes_tested,1,1),collapse = ".")
    proc_gender <- switch(input$proc_gender,
           "Males" = "Males",
           "Females" = "Females",
           "Males & Females" = "Both")
    name <- paste(lab_name,expr_design,genotypes_tested_init,proc_gender,humanDate(),sep = "_")
    if (is.null(genotypes_tested) || genotypes_tested=="")
      reset("experiment_identifier")
    else
      updateTextInput(session,"experiment_identifier",value = name)
  })
  
  ## when another procedure is selected i) update meta data editor ii) update measures table
  observeEvent(input$proc_name,{
    proc_name_selected <- input$proc_name 
    
    ## i) update meta data editor
    tbl_meta_selected <- tbl_meta %>%
      filter(procedure_name == proc_name_selected, parameter_name != "Standard Operating Procedure") %>%
      select("Parameter Name" = parameter_name, "Default Value" = default ,Units = parameter_unit) %>% 
      as.data.frame()
    
    tbl_meta_selected_txt <- paste0(capture.output(print(tbl_meta_selected,right = F, row.names = FALSE)),collapse="\n")
    
    if (nrow(tbl_meta_selected)==0)
      updateAceEditor(session,"meta_data_editor",value = ".",theme="ambiance",mode = "r")
    else
      updateAceEditor(session,"meta_data_editor",value = tbl_meta_selected_txt, theme="ambiance",mode = "r")
    
    ## ii) update measures table
    tbl_measures_selected <- tbl_measures %>%
      filter(procedure_name == proc_name_selected)
    
    measure_name_list <- as.character(tbl_measures_selected$parameter_name)
    
    updateSelectInput(session,"measure_name", choices = measure_name_list)
  })
  
  ## when another procedure is selected, render SOP link 
  output$proc_SOP_link <- renderText({
    proc_name_selected <- input$proc_name
    
    tbl_SOP_selected <- tbl_meta %>%
      filter(procedure_name == proc_name_selected, parameter_name == "Standard Operating Procedure") %>%
      transmute("Standard Operating Procedure" = default) %>% 
      as.data.frame()
    SOP_selected <- tbl_SOP_selected[1,1]
    
    if (is.na(SOP_selected))
      NULL
    else
      paste0("<a href='",SOP_selected,"' target='_blank'>Link to Standard Operating Procedure</a>")
    })
  
  ## when genotypes are selected, create a list with table to be filled with experiment results,
  ## genotypes select and experiment design:
  
  create_stats_tbl <- reactive({
    
    input$reset_stats_tbl
    input$reset_measure
    input$reset_proc
    input$reset_experiment
    expr_design <- input$expr_design
    
    if (expr_design=='Pairwise Comparisons')
    {
      genotypes_tested <- input$genotypes_tested_pairwise
      if (is.null(genotypes_tested))
        return ()
      else
      {
        html <- "<table><thead><tr><th>Group</th><th>Mean</th><th>SD</th><th>N</th></tr></thead><tbody>"
        for (g in genotypes_tested)
          html <- paste0(html,"<tr><td>",g,"</td><td><input class='tb' id='",g,"_mean' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_SD' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_N' type='number' value='' step='1'/></td></tr>")
        html <- paste0(html,"</tbody></table>")
        return(HTML(html))
      }
    }
    
    if (expr_design=='Control vs. Cases')
    {
      genotypes_tested <- c(input$genotypes_tested_control,input$genotypes_tested_cases)
      if (is.null(genotypes_tested))
        return ()
      else
      {
        html <- "<table><thead><tr><th>Group</th><th>Mean</th><th>SD</th><th>N</th></tr></thead><tbody>"
        g <- genotypes_tested[1]
        html <- paste0(html,"<tr><td>",g,"</td><td><input class='tb' id='",g,"_mean' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_SD' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_N' type='number' value='' step='1'/></td></tr>")
        html <- paste0(html,"<tr><td colspan='4'><hr></td></tr>")
        for (g in genotypes_tested[-1])
          html <- paste0(html,"<tr><td>",g,"</td><td><input class='tb' id='",g,"_mean' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_SD' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_N' type='number' value='' step='1'/></td></tr>")
        html <- paste0(html,"</tbody></table>")
        return(list(tbl_html=HTML(html),genotypes_tested = genotypes_tested , expr_design = expr_design))
      }
    }
  })
  
  output$stats_tbl <- renderUI(create_stats_tbl()$tbl_html)
  
  ## when measure is selected get meassure details
  selected_measure_details <- reactive({
    measure_name_selected <- input$measure_name
    measure_selected_vec <- tbl_measures %>% 
      filter(parameter_name==measure_name_selected) %>% 
      mutate(S2_ratio = S2_interaction/S2_error) %>% 
      as.data.frame() %>% 
      "["(1,)
    return(as.list(measure_selected_vec))
    })
  
  ## when meassure details retrived present S_int_2 coef units and transformation
  output$selected_measure_details <- renderUI({
    S2_ratio <- selected_measure_details()$S2_ratio %>% 
      round(4)
    parameter_trans_symbol <- selected_measure_details()$parameter_trans_symbol %>% 
      as.character()
    parameter_unit <- selected_measure_details()$parameter_unit %>% 
      as.character()
    withMathJax(HTML("\\(S^2_{int.}/S^2_{error}=\\) ",S2_ratio,
                     "<br><strong>Units:</strong> ",parameter_unit,
                     "<br><strong>Transformation:</strong> ",parameter_trans_symbol))
  })
  
# ---- reset operation ----

  observeEvent(input$reset_experiment, {
    reset("lab_name"); reset("proc_gender"); reset("expr_design") 
    reset("genotypes_tested_pairwise"); reset("genotypes_tested_control"); reset("genotypes_tested_cases")
    updateSelectizeInput(session,inputId = "proc_name",options = list(onInitialize = I('function() { this.setValue(""); }')))
    reset("proc_age")
    reset("proc_duration")
    runjs("$('textarea').val('')")
  })
  
  observeEvent(input$reset_proc, {
    updateSelectizeInput(session,inputId = "proc_name",options = list(onInitialize = I('function() { this.setValue(""); }')))
    reset("proc_age")
    reset("proc_duration")
    runjs("$('textarea').val('')")
  })
  
  observeEvent(input$reset_measure, {
    reset("measure_name")
  })
  
# ---- main function ----

  ## when form is submited, read data from stats table, create comparisons
  observeEvent(input$go,{
    genotypes_tested <- create_stats_tbl()$genotypes_tested
    expr_design <- create_stats_tbl()$expr_design
    measure_details <- selected_measure_details()
    
    S2_int <- measure_details$S2_interaction
    n_lab <- measure_details$n_lab
    n_genotype <- measure_details$n_genotype
   
    # read input to df
    genotypes_tested <- c("aaa","bbb","ccc")
    ids_vec <- c(paste0("input$'",genotypes_tested,"_mean'"),
                 paste0("input$'",genotypes_tested,"_SD'"),
                 paste0("input$'",genotypes_tested,"_N'"))
    input <- NULL
    stats_vec <- sapply(ids_vec, function(x)eval(parse(text=x)))
    stats_mat <- matrix(stats_vec,ncol = 3)
    
    ############## continue here###############
    
    if (expr_design=='Pairwise Comparisons')
    {
      gxl_adjust(type = ??,name_vec = genotypes_tested, mean_vec = stats_mat[,1], SD_vec = stats_mat[,3],
                 N_vec = stats_mat[,3], design = "pairwise", S2_int, n_lab, n_genotype)
    }
    
    if (expr_design=='Control vs. Cases')
    {
      gxl_adjust(type = ??, name_vec = genotypes_tested, mean_vec = stats_mat[,1], SD_vec = stats_mat[,3],
                 N_vec = stats_mat[,3], design = "control", S2_int, n_lab, n_genotype)
    }  
    
    
    
  })  
      

      
      
  # render results table: 
#   output$tbl_pv <- renderTable(maketable()[1:4],digits=c(0,0,6,6,6))
#   # render download handler: 
#   output$dh <- downloadHandler(filename = function() {paste0("Results_",gsub(" ", "_", input$Measure),".csv")},
#                                content = function(file) {write.csv(maketable(),file,row.names = FALSE)})
  
  # render plot (draft)-------------------------------------------------------------
  
  
  #   output$plot <- renderPlot({
  #     if (input$go==0)
  #       return()
  #     data <- maketable()[5:10]
  #     m1 <- data[,1]
  #     m2 <- data[,2]
  #     sp <- data[,3]
  #     sint <- data[,4]
  #     s <- data[,5]
  #     d <- abs(m1-m2)
  #     shift <- d*0.02
  #     plot(c(0,0,0,0),c(min(m1,m2),max(m1,m2),min(m1,m2)-0.277*abs(d),max(m1,m2)+0.277*abs(d)),
  #          xlim=c(-0.5,d),
  #          col=c(1,1,0,0),pch=19,
  #          main = input$Measure,xaxt="n",asp=1, axes=FALSE, frame.plot=F)
  #     axis(side = 2,pos = 0, col= "grey",las=2)
  #     segments(x0 = shift ,y0 = min(m1,m2),x1 = shift+sint*sp/s,y1 = min(m1,m2)+sint^2/s,col = rgb(159,100,100,maxColorValue = 255) ,lwd = 3 )
  #     segments(x0 = shift ,y0 = min(m1,m2)+s,x1 = shift + sint*sp/s,y1 = min(m1,m2)+sint^2/s,col = rgb(159,92,92,maxColorValue = 255) ,lwd = 3 )
  #     segments(x0 = 0 ,y0 = min(m1,m2),x1 = 0 ,y1 = max(m1,m2),
  #              col = if (d>s) rgb(120,165,68,maxColorValue = 255) else rgb(161,153,153,maxColorValue = 255),lwd = 3 )
  #     text(x = sint*sp/s+shift ,y = min(m1,m2)+sint^2/s + 0.5* (sp^2/s) , "Sp")
  #     text(x = sint*sp/s+shift ,y = min(m1,m2)+sint^2/s - 0.5*(sint^2/s), "Sint.")
  #     
  #   })
  
  
  # reset buttons -----------------------------------------------------------
  
#   observe({
#     if (input$Re)
#     {
#       isolate({
#         for (g in input$Genotypes)
#         {
#           updateTextInput(session, inputId = paste0(g,".mean"),value = "")
#           updateTextInput(session, inputId = paste0(g,".SD"),value = "")
#           updateTextInput(session, inputId = paste0(g,".N"),value = "")
#         }
#         
#         #1       
#         s_options <- list()
#         updateTextInput(session, "ExperimentName",value = "")
#         updateTextInput(session, "LabAddress", ,value = "")
#         updateSelectizeInput(session, "Genotypes", choices = Genotypes, 
#                              options = list(create = TRUE, maxOptions = 5,
#                                             placeholder = 'Select from the list or add one',onInitialize = I('function() { this.setValue(""); }')))
#         #2
#         updateDateInput(session,inputId =  "date", value = date())
#         updateNumericInput(session,inputId =  "Age", value = "")
#         updateRadioButtons(session,inputId =  "Sex",selected = "Males")
#         updateSelectInput(session, "TestType",selected = "-")
#         updateRadioButtons(session, "ArenaS",selected = "rectangle")
#         updateNumericInput(session, "AW", value = "")
#         updateNumericInput(session, "AL", value = "")
#         updateNumericInput(session, "AR", value = "")
#         updateNumericInput(session, "AH", value = "")
#         updateSelectizeInput(session, "AM", choices = c("disposable floor paper","non-reflective white plastic"),options = list(create = TRUE, placeholder = 'Select a material or add one',onInitialize = I('function() { this.setValue(""); }')))
#         updateNumericInput(session, "TL", value = "")
#         updateRadioButtons(session, "TM", selected = "video")
#         
#         updateNumericInput(session,"Eaf",value = "")
#         updateNumericInput(session,"Wh",value = "")
#         updateSelectizeInput(session,"Ct", choices = c("Macrolon type II","Macrolon type III"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select type or add one',onInitialize = I('function() { this.setValue(""); }')))
#         updateNumericInput(session,"Bh",value = "")
#         updateNumericInput(session,"Bw",value = "")
#         updateSelectizeInput(session,"Pm", choices = c("wood wrapped with cloth tape"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select material or add one',onInitialize = I('function() { this.setValue(""); }')))
#         updateNumericInput(session,"Bh",value = "")
#         updateNumericInput(session,"Bw",value = "")
#         
#         
#         updateSelectizeInput(session, "TS",  choices = c("EthoVision <3.0",
#                                                          "EthoVision 3.0",
#                                                          "EthoVision 3.1",
#                                                          "EthoVision XT",
#                                                          "ANYmaze"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select a system or add one',onInitialize = I('function() { this.setValue(""); }')))
#         
#         updateRadioButtons(session,"Phase",selected = "light")
#         updateNumericInput(session,"TD", value = "")
#         
#         #3
#         
#         
#         TT <- input$TestType
#         m_choices <- as.list(dat$Meassure[dat$Test==TT])
#         names(m_choices) <- paste0(dat$Meassure[dat$Test==TT]," [",dat$unit[dat$Test==TT],",",dat$trans[dat$Test==TT],"]")
#         updateSelectInput(session,"Measure",choices = m_choices)
#         
#         
#       })
#       
#     }
#     else
#       return()
#   })
#   
#   observe({
#     if (input$Rt)
#     {
#       isolate({
#         
#         for (g in input$Genotypes)
#         {
#           updateTextInput(session, inputId = paste0(g,".mean"),value = "")
#           updateTextInput(session, inputId = paste0(g,".SD"),value = "")
#           updateTextInput(session, inputId = paste0(g,".N"),value = "")
#         }
#         
#         #2
#         updateDateInput(session,inputId =  "date", value = date())
#         updateNumericInput(session,inputId =  "Age", value = "")
#         updateRadioButtons(session,inputId =  "Sex",selected = "Males")
#         updateSelectInput(session, "TestType",selected = "-")
#         updateRadioButtons(session, "ArenaS",selected = "rectangle")
#         updateNumericInput(session, "AW", value = "")
#         updateNumericInput(session, "AL", value = "")
#         updateNumericInput(session, "AR", value = "")
#         updateNumericInput(session, "AH", value = "")
#         updateSelectizeInput(session, "AM", choices = c("disposable floor paper","non-reflective white plastic"),options = list(create = TRUE, placeholder = 'Select a material or add one',onInitialize = I('function() { this.setValue(""); }')))
#         updateNumericInput(session, "TL", value = "")
#         updateRadioButtons(session, "TM", selected = "video")
#         
#         updateNumericInput(session,"Eaf",value = "")
#         updateNumericInput(session,"Wh",value = "")
#         updateSelectizeInput(session,"Ct", choices = c("Macrolon type II","Macrolon type III"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select type or add one',onInitialize = I('function() { this.setValue(""); }')))
#         updateNumericInput(session,"Bh",value = "")
#         updateNumericInput(session,"Bw",value = "")
#         updateSelectizeInput(session,"Pm", choices = c("wood wrapped with cloth tape"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select material or add one',onInitialize = I('function() { this.setValue(""); }')))
#         updateNumericInput(session,"Bh",value = "")
#         updateNumericInput(session,"Bw",value = "")
#         
#         
#         updateSelectizeInput(session, "TS",  choices = c("EthoVision <3.0",
#                                                          "EthoVision 3.0",
#                                                          "EthoVision 3.1",
#                                                          "EthoVision XT",
#                                                          "ANYmaze"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select a system or add one',onInitialize = I('function() { this.setValue(""); }')))
#         
#         updateRadioButtons(session,"Phase",selected = "light")
#         updateNumericInput(session,"TD", value = "")
#         
#         #3
#         
#         
#         TT <- input$TestType
#         m_choices <- as.list(dat$Meassure[dat$Test==TT])
#         names(m_choices) <- paste0(dat$Meassure[dat$Test==TT]," [",dat$unit[dat$Test==TT],",",dat$trans[dat$Test==TT],"]")
#         updateSelectInput(session,"Measure",choices = m_choices)
#         
#         
#       })
#       
#     }
#     else
#       return()
#   })
#   
#   observe({
#     if (input$Rm)
#     {
#       isolate({
#         
#         for (g in input$Genotypes)
#         {
#           updateTextInput(session, inputId = paste0(g,".mean"),value = "")
#           updateTextInput(session, inputId = paste0(g,".SD"),value = "")
#           updateTextInput(session, inputId = paste0(g,".N"),value = "")
#         }
#         
#         #3
#         
#         
#         TT <- input$TestType
#         m_choices <- as.list(dat$Meassure[dat$Test==TT])
#         names(m_choices) <- paste0(dat$Meassure[dat$Test==TT]," [",dat$unit[dat$Test==TT],",",dat$trans[dat$Test==TT],"]")
#         updateSelectInput(session,"Measure",choices = m_choices)
#         
#         
#         
#       })
#       
#     }
#     else
#       return()
#   })
#   
#   observe({
#     if (input$Rr)
#     {
#       isolate({
#         
#         for (g in input$Genotypes)
#         {
#           updateTextInput(session, inputId = paste0(g,".mean"),value = "")
#           updateTextInput(session, inputId = paste0(g,".SD"),value = "")
#           updateTextInput(session, inputId = paste0(g,".N"),value = "")
#         }
#         
#       })
#       
#     }
#     else
#       return()
#   })
#   
#   observe({
#     input$Measure
#     for (g in input$Genotypes)
#     {
#       updateTextInput(session, inputId = paste0(g,".mean"),value = "")
#       updateTextInput(session, inputId = paste0(g,".SD"),value = "")
#       updateTextInput(session, inputId = paste0(g,".N"),value = "")
#     }
#   })
  # Examples ----------------------------------------------------------------
  
  ## Example 1:
  
#   observe({
#     if (input$Example1a)
#     {
#       isolate({
#         
#         updateTextInput(session, "ExperimentName",value = "Example_1_OFT")
#         updateTextInput(session, "LabAddress", ,value = "Utrecht")
#         updateSelectizeInput(session, "Genotypes", selected = c("C57BL/6N","DBA/2"))
#         
#         updateDateInput(session,inputId =  "date", value = "2011-01-01")
#         updateNumericInput(session,inputId =  "Age", value = "12")
#         updateRadioButtons(session,inputId =  "Sex",selected = "Females")
#         updateSelectInput(session, "TestType",selected = "Open field test")
#         updateRadioButtons(session, "ArenaS",selected = "rectangle")
#         updateNumericInput(session, "AW", value = 50)
#         updateNumericInput(session, "AL", value = 50)
#         updateNumericInput(session, "AR", value = "")
#         updateNumericInput(session, "AH", value = 37)
#         updateSelectizeInput(session, "AM", selected = "disposable floor paper")
#         updateNumericInput(session, "TL", value = 60)
#         updateRadioButtons(session, "TM", selected = "video")
#         updateSelectizeInput(session, "TS",  selected = c("EthoVision 3.1"))
#         
#         updateRadioButtons(session,"Phase",selected = "light")
#         updateNumericInput(session,"TD", value = 10)
#         updateSelectInput(session,inputId = "Measure",selected = "bolus count")
#         
#       })
#     }
#     else
#       return()
#   })
#   
#   observe({
#     if (input$Example1b)
#     {
#       isolate({
#         updateTextInput(session, inputId = "C57BL/6N.mean",value = 4.438)
#         updateTextInput(session, inputId = "C57BL/6N.SD",value = 3.204)
#         updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
#         updateTextInput(session, inputId = "DBA/2.mean",value = 10.938)
#         updateTextInput(session, inputId = "DBA/2.SD",value = 4.074)
#         updateTextInput(session, inputId = "DBA/2.N",value = 16)
#       })
#     }
#     else
#       return()
#     
#   })
#   
#   
#   ## Example 2:
#   
#   observe({
#     if (input$Example2a)
#     {
#       isolate({
#         
#         updateTextInput(session, "ExperimentName",value = "Example_2_OFT")
#         updateTextInput(session, "LabAddress", ,value = "Giessen")
#         updateSelectizeInput(session, "Genotypes", selected = c("C57BL/6N","DBA/2"))
#         
#         updateDateInput(session,inputId =  "date", value = "2011-01-01")
#         updateNumericInput(session,inputId =  "Age", value = "12")
#         updateRadioButtons(session,inputId =  "Sex",selected = "Females")
#         updateSelectInput(session, "TestType",selected = "Open field test")
#         updateRadioButtons(session, "ArenaS",selected = "rectangle")
#         updateNumericInput(session, "AW", value = 50)
#         updateNumericInput(session, "AL", value = 50)
#         updateNumericInput(session, "AR", value = "")
#         updateNumericInput(session, "AH", value = 37)
#         updateSelectizeInput(session, "AM", selected = "disposable floor paper")
#         updateNumericInput(session, "TL", value = 60)
#         updateRadioButtons(session, "TM", selected = "video")
#         updateSelectizeInput(session, "TS",  selected = c("EthoVision 3.1"))
#         
#         updateRadioButtons(session,"Phase",selected = "light")
#         updateNumericInput(session,"TD", value = 10)
#         updateSelectInput(session,inputId = "Measure",selected = "total path moved")
#         
#       })
#     }
#     else
#       return()
#   })
#   
#   observe({
#     if (input$Example2b)
#     {
#       isolate({
#         updateTextInput(session, inputId = "C57BL/6N.mean",value = 59.861)
#         updateTextInput(session, inputId = "C57BL/6N.SD",value = 5.977)
#         updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
#         updateTextInput(session, inputId = "DBA/2.mean",value = 70.768)
#         updateTextInput(session, inputId = "DBA/2.SD",value = 14.279)
#         updateTextInput(session, inputId = "DBA/2.N",value = 16)
#       })
#     }
#     else
#       return()
#     
#   })
#   
#   ## Example 3:
#   
#   observe({
#     if (input$Example3a)
#     {
#       isolate({
#         
#         updateTextInput(session, "ExperimentName",value = "Example_3_OFT_multi")
#         updateTextInput(session, "LabAddress", ,value = "Giessen")
#         updateSelectizeInput(session, "Genotypes", selected = c("C57BL/6N","DBA/2"))
#         
#         updateDateInput(session,inputId =  "date", value = "2011-01-01")
#         updateNumericInput(session,inputId =  "Age", value = "12")
#         updateRadioButtons(session,inputId =  "Sex",selected = "Females")
#         updateSelectInput(session, "TestType",selected = "Open field test")
#         updateRadioButtons(session, "ArenaS",selected = "rectangle")
#         updateNumericInput(session, "AW", value = 50)
#         updateNumericInput(session, "AL", value = 50)
#         updateNumericInput(session, "AR", value = "")
#         updateNumericInput(session, "AH", value = 37)
#         updateSelectizeInput(session, "AM", selected = "disposable floor paper")
#         updateNumericInput(session, "TL", value = 60)
#         updateRadioButtons(session, "TM", selected = "video")
#         updateSelectizeInput(session, "TS",  selected = c("EthoVision 3.1"))
#         
#         updateRadioButtons(session,"Phase",selected = "light")
#         updateNumericInput(session,"TD", value = 10)
#         updateSelectInput(session,inputId = "Measure",selected = "path moved within centre")
#         
#       })
#     }
#     else
#       return()
#   })
#   
#   observe({
#     if (input$Example3b)
#     {
#       isolate({
#         updateTextInput(session, inputId = "C57BL/6N.mean",value = 49.513)
#         updateTextInput(session, inputId = "C57BL/6N.SD",value = 6.411)
#         updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
#         updateTextInput(session, inputId = "DBA/2.mean",value = 56.814)
#         updateTextInput(session, inputId = "DBA/2.SD",value = 17.330)
#         updateTextInput(session, inputId = "DBA/2.N",value = 16)
#       })
#     }
#     else
#       return()
#     
#   })
#   
#   
#   ## Example 4:
#   
#   observe({
#     if (input$Example4a)
#     {
#       isolate({
#         
#         updateTextInput(session, "ExperimentName",value = "Example_4_NOT")
#         updateTextInput(session, "LabAddress", ,value = "Mannhein")
#         updateSelectizeInput(session, "Genotypes", selected = c("C57BL/6N","DBA/2"))
#         
#         updateDateInput(session,inputId =  "date", value = "2012-02-02")
#         updateNumericInput(session,inputId =  "Age", value = "12")
#         updateRadioButtons(session,inputId =  "Sex",selected = "Females")
#         updateSelectInput(session, "TestType",selected = "Novel object test")
#         updateNumericInput(session, "TL", value = 60)
#         updateRadioButtons(session, "TM", selected = "video")
#         updateSelectizeInput(session, "TS",  selected = c("EthoVision 3.1"))
#         
#         updateRadioButtons(session,"Phase",selected = "light")
#         updateNumericInput(session,"TD", value = 10)
#         updateSelectInput(session,inputId = "Measure",selected = "path moved within exploration zone 1")
#         updateSelectInput(session,inputId = "Measure",selected = "path moved within exploration zone 1")
#         
#       })
#       
#     }
#     else
#       return()
#   })
#   
#   observe({
#     if (input$Example4b)
#     {
#       isolate({
#         updateTextInput(session, inputId = "C57BL/6N.mean",value = 15.544)
#         updateTextInput(session, inputId = "C57BL/6N.SD",value = 5.192)
#         updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
#         updateTextInput(session, inputId = "DBA/2.mean",value = 20.346)
#         updateTextInput(session, inputId = "DBA/2.SD",value = 7.783)
#         updateTextInput(session, inputId = "DBA/2.N",value = 16)
#       })
#     }
#     else
#       return()
#   })
  
})


# output$tbl_pv_TIC <- renderTable({
#   if (input$go)
#   {
#     isolate({
#       
#       #         if (input$Measures == 'Time in center (in min.)' || input$Measures[2] == 'Time in center (in min.)')
#       #         {
#       means <- matrix(sapply(paste0("input$'",combn(input$Genotypes, 2),".mean.TIC'"), function(x)eval(parse(text=x))) ,ncol = 2,byrow = T)
#       SD <- matrix(sapply(paste0("input$'",combn(input$Genotypes, 2),".SD.TIC'"), function(x)eval(parse(text=x))) ,ncol = 2,byrow = T)
#       N <- matrix(sapply(paste0("input$'",combn(input$Genotypes, 2),".N.TIC'"), function(x)eval(parse(text=x))) ,ncol = 2,byrow = T)
#       
#       D <- means[,1] - means[,2]
#       Sp2 <- ((N[,1]-1)*SD[,1]^2+(N[,2]-1)*SD[,2]^2)/(N[,1]+N[,2]-2)
#       Tstat1 <- abs(D)/sqrt(Sp2*(1/N[,1]+1/N[,2]))
#       Tstat2 <- abs(D)/sqrt(Sp2*(1/N[,1]+1/N[,2])+2*Sint2.TIC)
#       pv <- 2-2*pt(q = Tstat1,df = N[,1]+N[,2]-2) 
#       ni <- 1/ ((Sp2*(1/N[,1]+1/N[,2]))^2/(N[,1]+N[,2]-2)+(2*Sint2.TIC)^2/(L-1)/(S-1)) * (Sp2*(1/N[,1]+1/N[,2])+2*Sint2.TIC)^2
#       rv <- 2-2*pt(q = Tstat2,df = ni) 
#       
#       pair <- apply(combn(input$Genotypes, 2),2,function(x) paste(x[1],x[2],sep = " : "))
#       data.frame(pair = pair,Diff=D,"p-value" = pv,"r-value" = rv)
#       #         }
#       #         else NULL
#       
#       
#     })
#   }
#   else NULL
#   
# },digits=c(0,0,4,4,4))

#   output$tbl_TIC <- renderUI({
#     html <- "<table><thead><tr><th>Group</th><th>Group Mean</th><th>Group SD</th><th>Group N</th></tr></thead><tbody>"
#     for (g in input$Genotypes)
#       html <- paste0(html,"<tr><td>",g,"</td><td><input class='tb' id='",g,".mean.TIC' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,".SD.TIC' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,".N.TIC' type='number' value='' step='1'/></td></tr>")
#     html <- paste0(html,"</tbody></table>")
#     return(HTML(html))})



