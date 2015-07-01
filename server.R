library(shiny)
library(dplyr)
library(shinyIncubator)
library(shinyAce)
library(shinyjs)
library(DT)

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
    
    if (expr_design=='Tukey')
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
        return(list(tbl_html=HTML(html),genotypes_tested = genotypes_tested , expr_design = expr_design))
      }
    }
    
    if (expr_design=='Dunnet')
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
  get_comparisons_object <- eventReactive(input$submit_data,{
    
    # read data:
    stats_tbl <- create_stats_tbl()
    measure_details <- selected_measure_details()
    
    genotypes_tested <- stats_tbl$genotypes_tested
    expr_design <- stats_tbl$expr_design
   
    S2_int <- measure_details$S2_interaction
    n_lab <- measure_details$n_lab
    n_genotype <- measure_details$n_genotype
    
    type <- ifelse(input$mult_correct,"single-step","none")
    alpha <- input$alpha
    
    # read input to df
    ids_vec <- c(paste0("input$'",genotypes_tested,"_mean'"),
                 paste0("input$'",genotypes_tested,"_SD'"),
                 paste0("input$'",genotypes_tested,"_N'"))
    
    stats_vec <- sapply(ids_vec, function(x)eval(parse(text=x)))
    stats_mat <- matrix(stats_vec,ncol = 3)
    
    # create comparisons object
    co <- gxl_adjust(type = type, name_vec = genotypes_tested, mean_vec = stats_mat[,1], SD_vec = stats_mat[,3],
                     N_vec = stats_mat[,3], design = expr_design, S2_int, n_lab, n_genotype, alpha = alpha)
    tbl_res <- get_res_table(co)
    
    return(list(co=co,tbl_res=tbl_res,measure_details=measure_details,stats_mat=stats_mat,expr_design=expr_design,genotypes_tested=genotypes_tested))
  })
  
  observeEvent(input$submit_data, {
    shinyjs::show("results_sec")
  })
  
  ## when main object created, use it to print table
  output$out_tbl <- DT::renderDataTable({
    o <<- get_comparisons_object()
    tbl_res <- o$tbl_res
    
    # table sketch
    sketch <- withTags(table(
      class = 'display cell-border nowrap compact',
      thead(
        tr(
          th(rowspan = 2, 'Pair'),
          th(rowspan = 2, HTML('Unadjusted<br> p-value')),
          th(rowspan = 2, HTML('GxL adjusted<br> p-value')),
          th(rowspan = 2, 'Difference'),
          th(colspan = 2, 'Unadjusted CI'),
          th(colspan = 2, 'GxL adjusted CI')
        ),
        tr(
          lapply(rep(c('lwr', 'upr'), 2), th)
        )
      )
    ))
  
    # calculate siginificant digits for table (like display in xtable)
    sig_digit_est <- 
      c(tbl_res[,5]-tbl_res[,4],tbl_res[,3:5]) %>% # take the CIs lengths and all estimates values
      abs() %>% min() %>%                                      # the the minimum of thier absolute values 
      log(10) %>% {3-floor(.)} %>%                             # get its approximate order of magnitude, plus 3 is the desired digits to display
      max(3)                                                   # or 3 (the maximum of both)
    sig_digit_pv <- 
      tbl_res[,1] %>% min() %>%                            # take the minimum pv  abs() %>% min() %>% # the the minimum of thier absolute values 
      log(10) %>% {2-floor(.)} %>%                             # get its approximate order of magnitude, plus 3 is the desired digits to display
      max(4)                                                   # or 4 (the maximum of both)
    
    # create DT object
    caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;padding;0;',
      em(paste("Measure:",o$measure_details$parameter_name))
    )
    
    datatable(tbl_res,caption = caption,
              selection = "none", container = sketch,escape = T,
              options = list(paging =  F, dom='t', ordering = F, processing = T)) %>% 
      formatRound(1:2,sig_digit_pv) %>% 
      formatRound(3:8,sig_digit_est)
  })
  
  ## when main object created, plot cis
  output$ci_plot <- renderPlot({
    o <- get_comparisons_object()
    plot_confint_glht(o$co$ci,o$co$ci_new,xlab = o$measure_details$parameter_name)
  })
  
  output$dia_plot <- renderPlot({
    o <- get_comparisons_object()
    tbl_res <- o$tbl_res[,1:2]
    pair_names <- matrix(unlist((strsplit(rownames(tbl_res)," - "))),ncol=2,byrow = T)
    tbl_pairs <- data.frame(name1 = pair_names[,1],name2 = pair_names[,2],tbl_res)
    tbl_singles <- data.frame(name=o$genotypes_tested,mean=o$stats_mat[,1])
    plot_diagram(tbl_singles,tbl_pairs)
  })
  
  
  
#   output$dh <- downloadHandler(filename = function() {paste0("Results_",gsub(" ", "_", input$Measure),".csv")},
#                                content = function(file) {write.csv(maketable(),file,row.names = FALSE)})
#     
  
  ## Example 1:
  
  observeEvent(input$Example1_step1,{
    updateTextInput(session, "lab_name",value = "my_lab")
    updateRadioButtons(session,"expr_design",selected = "Tukey")
    updateSelectizeInput(session, "genotypes_tested_pairwise", selected = c("C57BL/6N","DBA/2","AKR/J"))
    updateRadioButtons(session,inputId =  "proc_gender",selected = "Females")
    updateSelectizeInput(session, "proc_name", selected = "Open Field Test")
    updateNumericInput(session,inputId =  "proc_age", value = "12")
    updateNumericInput(session,"proc_duration", value = 10)
    updateSelectInput(session,inputId = "measure_name",selected = "bolus count")
    updateSelectInput(session,inputId = "measure_name",selected = "bolus count")
  })
  
  observeEvent(input$Example1_step2,{
    updateTextInput(session, inputId = "C57BL/6N_mean",value = 4.438)
    updateTextInput(session, inputId = "C57BL/6N_SD",value = 3.204)
    updateTextInput(session, inputId = "C57BL/6N_N",value = 16)
    updateTextInput(session, inputId = "DBA/2_mean",value = 10.938)
    updateTextInput(session, inputId = "DBA/2_SD",value = 4.074)
    updateTextInput(session, inputId = "DBA/2_N",value = 16)
    updateTextInput(session, inputId = "AKR/J_mean",value = 15)
    updateTextInput(session, inputId = "AKR/J_SD",value = 3)
    updateTextInput(session, inputId = "AKR/J_N",value = 12)
  })
})
  
#   
#   ## Example 2:
#   "total path moved"
#   updateTextInput(session, inputId = "C57BL/6N.mean",value = 59.861)
#   updateTextInput(session, inputId = "C57BL/6N.SD",value = 5.977)
#   updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
#   updateTextInput(session, inputId = "DBA/2.mean",value = 70.768)
#   updateTextInput(session, inputId = "DBA/2.SD",value = 14.279)
#   updateTextInput(session, inputId = "DBA/2.N",value = 16)
#   "path moved within centre"
#   updateTextInput(session, inputId = "C57BL/6N.mean",value = 49.513)
#   updateTextInput(session, inputId = "C57BL/6N.SD",value = 6.411)
#   updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
#   updateTextInput(session, inputId = "DBA/2.mean",value = 56.814)
#   updateTextInput(session, inputId = "DBA/2.SD",value = 17.330)
#   updateTextInput(session, inputId = "DBA/2.N",value = 16)
#   updateSelectInput(session,inputId = "Measure",selected = "path moved within exploration zone 1")
#   updateTextInput(session, inputId = "C57BL/6N.mean",value = 15.544)
#   updateTextInput(session, inputId = "C57BL/6N.SD",value = 5.192)
#   updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
#   updateTextInput(session, inputId = "DBA/2.mean",value = 20.346)
#   updateTextInput(session, inputId = "DBA/2.SD",value = 7.783)
#   updateTextInput(session, inputId = "DBA/2.N",value = 16)
#   