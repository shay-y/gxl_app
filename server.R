
function(input, output, session) {
  observe(label="console",{
    if(input$console != 0) {
      options(browserNLdisabled=TRUE)
      saved_console<-".RDuetConsole"
      if (file.exists(saved_console)) load(saved_console)
      isolate(browser())
      save(file=saved_console,list=ls(environment()))
    }
  })
  
  values <- reactiveValues(
    genotypes_tested = NULL,
    tbl_metadata_selected = NULL,
    tbl_measures_selected = NULL,
    measure_name_list = NULL,
    tbl_summaries = NULL,
    tbl_raw_input = NULL)

  ## metadata input fields
  output$metadata_input <- renderUI({
    req(input$procedure_name)
    values$tbl_metadata_selected <- tbl_metadata_1 %>% 
      filter(procedure_name == input$procedure_name)
    tags$table(
      tags$thead(
        tags$tr(
          tags$th("Parameter name"),tags$th("Value"),tags$th("Unit")
        )
      ),
      tags$tbody(
        apply(
          values$tbl_metadata_selected,1,
          function(row)
          {
            tags$tr(
              tags$td(row$parameter_name),
              tags$td(
                selectizeInput(
                  inputId = row$parameter_stable_id,
                  label = NULL,
                  choices = {row$options %>% {setNames(.,.)}},
                  options = list(create = TRUE))),
              tags$td(row$unit)
            )
          }
        )
      )
    )
  })
  
  ## measures input
  observeEvent(input$procedure_name,{
    req(input$procedure_name)
    
    values$tbl_measures_selected <-
      tbl_measure_1 %>%
      filter(procedure_name == input$procedure_name)
    
    values$measure_name_list <-
      values$tbl_measures_selected %>% 
      .$parameter_name %>%
      as.character() %>%
      as.list() %>%
      {setNames(.,.)}
    updateSelectInput(session,"measure_name", choices = values$measure_name_list)
  })
  
  ## SOP link 
  output$proc_SOP_link <- renderUI({
    req(input$procedure_name)
    
    selected_url <- 
      tbl_procedures_1 %>% 
      filter(procedure_name == input$procedure_name) %>%
      select(url) %>% 
      .$url
    
    a(href = selected_url,
      target='_blank',
      "Link to Protocol")
  })
  
  ## when measue is selected ->> print measure details
  output$selected_measure_details <- renderUI({
    req(input$measure_name,input$procedure_name)
    values$measure_selected <-
      tbl_measure_1 %>%
      filter(procedure_name == input$procedure_name &
               parameter_name==input$measure_name)
    with(
      values$measure_selected,
      withMathJax(
        HTML(
          "\\(S^2_{int.}/S^2_{error}=\\)",
          s2_ratio %>% format(digits=5,nsmall=3),
          "<br><strong>Units:</strong> ",
          unit,
          "<br><strong>Transformation:</strong> ",
          transformation_symbol
        )
      )
    )
  })
  
  output$file_input <- renderUI({
    input$reset_upload
    fileInput(
      inputId = "upload_file",
      label = "Upload CSV file (no header):",
      accept = 'text/csv',
      width="80%")
  })
  
  observeEvent(input$reset_upload,{
    values$tbl_raw_input <- NULL
    values$tbl_summaries <- NULL
    reset("groups")
  }
  #priority = 1,
  #suspended = T
  )
  
  ## when file is selected ->> read raw table 
  observeEvent(input$upload_file,{
    values$tbl_raw_input <- 
      read.csv(
        file = input$upload_file$datapath,
        header = F,
        stringsAsFactors = F
      )
  },priority = -1)  
  
  ## upon raw table upload ->> create summaries table with and without transformation
  observeEvent(values$tbl_raw_input,{
    req(values$measure_selected$transformation_function)
    trans_fun <- eval(parse(text=paste("function(x)",values$measure_selected$transformation_function)))

    if(is.null(values$tbl_raw_input))
      values$tbl_summaries <- NULL
    else
    {
      values$tbl_summaries <- 
        values$tbl_raw_input  %>% 
        mutate(transformed = trans_fun(V2)) %>% 
        group_by(V1) %>% 
        summarise(mean_t = mean(transformed,na.rm = T),
                  mean = mean(V2,na.rm = T),
                  sd_t =sd(transformed,na.rm = T),
                  sd =sd(V2,na.rm = T),
                  n =sum(!is.na(transformed)))
    }
  })
 
  ## ...and render the table as HTML, + add rows for summaries after transformation
  output$input_summaries <- renderUI({
    tags$table(
      tags$thead(
        tags$tr(
          tags$th("Group"),tags$th("Mean"),tags$th("Standard Deviation"),tags$th("Num. of Observations")
        )
      ),   
      tags$tbody(
        if (!is.null(values$tbl_summaries))
        {
          values$tbl_summaries %>%
            add_rownames() %>% 
            rowwise() %>% 
            transmute(
              html_row =
                tags$tr(   
                  tags$td(V1),
                  tags$td(numericInput(inputId = paste(rowname,"mean","."), label = NULL, value = mean_t)),
                  tags$td(numericInput(inputId = paste(rowname,"sd","."), label = NULL, value = sd_t, min = 0)),
                  tags$td(numericInput(inputId = paste(rowname,"n","."), label = NULL, value = n, step = 1,min = 0))
                ) %>%
                as.character()
            ) %>% 
            .$html_row %>% 
            HTML()
        } else NULL,
        
        if(!is.null(input$groups))
        {
          lapply(
            1:length(input$groups),function(gene_id)
            {
              tags$tr(
                tags$td(input$groups[gene_id]),
                tags$td(numericInput(inputId = paste(gene_id,"mean","."), label = NULL, value = "")),
                tags$td(numericInput(inputId = paste(gene_id,"sd","."), label = NULL, value = "", min = 0)),
                tags$td(numericInput(inputId = paste(gene_id,"n","."), label = NULL, value = "", step = 1,min = 0))
              )
            }
          )
        } else NULL
      )
    )
  })
  
  ## workaroud to link to file using relative path (the example data to download)
  output$example_raw_input <- downloadHandler(
    filename = "Simplified IPGTT Glucose response AUC - example.csv",
    content = function(con) {
      write.table(x = tbl_example_measure_input,file = con,row.names = F,col.names = F, qmethod = "double",sep = ",")
    }
  )
  
    
# ---- reset operations ----
# 
#   observeEvent(input$reset_experiment, {
#     reset("lab_name")
#     reset("proc_gender") 
#     updateSelectizeInput(session,"genotypes_tested_pairwise", options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
#     updateSelectizeInput(session,"genotypes_tested_control", options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list one',onInitialize = I('function() { this.setValue(""); }')))
#     updateSelectizeInput(session,"genotypes_tested_cases", options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
#     reset("expr_design")
#     
#     updateSelectizeInput(session,inputId = "proc_name",options = list(onInitialize = I('function() { this.setValue(""); }')))
#     reset("proc_age")
#     reset("proc_duration")
#     runjs("$('textarea').val('')")
#   })
#   
#   observeEvent(input$reset_proc, {
#     updateSelectizeInput(session,inputId = "proc_name",options = list(onInitialize = I('function() { this.setValue(""); }')))
#     reset("proc_age")
#     reset("proc_duration")
#     runjs("$('textarea').val('')")
#   })
#   
#   observeEvent(input$reset_measure, {
#     reset("measure_name")
#   })
  
# ---- main function ----

# as ----------------------------------------------------------------------


#  ------------------------------------------------------------------------


#  ------------------------------------------------------------------------


#  ------------------------------------------------------------------------


# a -----------------------------------------------------------------------


  ## when form is submited, read data from stats table, create comparisons
#   get_comparisons_object <- eventReactive(input$submit_data,{
#     
#     # read data:
#     measure_details <- selected_measure_details()
#     
#     genotypes_tested <- values$genotypes_tested
#     expr_design <- input$expr_design
#     genotypes_tested <- values$genotypes_tested
#     S2_int <- measure_details$S2_interaction
#     n_lab <- measure_details$n_lab
#     n_genotype <- measure_details$n_genotype
#     
#     type <- ifelse(input$mult_correct,"single-step","none")
#     alpha <- input$alpha
# 
#     # read input to df
#     ids_vec <- c(paste0("input$'",genotypes_tested,"_mean'"),
#                  paste0("input$'",genotypes_tested,"_SD'"),
#                  paste0("input$'",genotypes_tested,"_N'"))
#     
#     stats_vec <- unlist(sapply(ids_vec, function(x)eval(parse(text=x))))
#     if (is.null(stats_vec))
#       return(NULL)
#     stats_matrix <- matrix(stats_vec,ncol = 3)
#     colnames(stats_matrix) <- c("mean","SD","N")
#     rownames(stats_matrix) <- genotypes_tested
#       
#     # create comparisons object
#     co <- gxl_adjust(type = type, name_vec = genotypes_tested,
#                      mean_vec = stats_matrix[,1], SD_vec = stats_matrix[,2], N_vec = stats_matrix[,3],
#                      design = expr_design, S2_int, n_lab, n_genotype, alpha = alpha)
#     tbl_res <- get_res_table(co)
#     
#     measure_list <- measure_details[c("Gender","parameter_id","procedure_name","parameter_name","S2_interaction","parameter_trans_symbol","parameter_unit","S2_ratio")]
#     details <- sapply(measure_list,as.character)
#     
#     return(list(
#       measure_details=measure_details,
#       tbl_res=tbl_res,
#       co=co,
#       stats_matrix=stats_matrix,
#       genotypes_tested = genotypes_tested,
#       expr_design = expr_design,
#       alpha=alpha,
#       input_stats_matrix = stats_matrix,
#       details = details,
#       results_table=tbl_res))
#   },ignoreNULL = F)
#   
#   ## when main object created, use it to print table
#   output$out_tbl <- DT::renderDataTable({
#     input$alpha # invalidate when alpha is changed (for immidiate reactivity)
#     o <- get_comparisons_object()
#     if (is.null(o))
#     {
#       tbl_NAs <- data.frame(as.list(rep(NA,7)))
#       colnames(tbl_NAs) <- c("p_value","p_value_adj", "est", "ci_lwr", "ci_upr",  "ci_lwr_adj", "ci_upr_adj")
#       rownames(tbl_NAs) <- "-"
#       get_datatable(tbl_NAs,NULL)
#     }else{
#       get_datatable(o$tbl_res,o$measure_details$parameter_name)
#     }
#   })
#   
#   outputOptions(output,"out_tbl",suspendWhenHidden = F)
#   
#   ## when main object created, plot diagram
#   observe({
#     o <- get_comparisons_object()
#     if(!is.null(o))
#     {
#       bind_shiny(vis = reactive({
#         
#         # load("o.Rdata")
#         
#         
#         tbl_res <- o$tbl_res[,1:3]
#         pair_names <- matrix(unlist((strsplit(rownames(tbl_res)," - "))),ncol=2,byrow = T)
#         tbl_pairs <- data.frame(name1 = pair_names[,1],name2 = pair_names[,2],tbl_res)
#         tbl_singles <- data.frame(name=o$genotypes_tested,mean=o$stats_matrix[,1])
#         plot_diagram(tbl_singles,tbl_pairs,alpha = 0.05, xMeasure = as.character(o$measure_details$parameter_name))
#       }), plot_id = "dia_plot",session = session, ,bg = "white", ,background = "white")
#     }
#   })
#   
# 
#   observe({
#     if (input$submit_data)
#       show("dia_plot_h")
#   })
#   
#   output$dia_plot_ph <- renderImage({
#     list(src = "WWW/placeholder1.svg",alt = "...")
#   },deleteFile = F)
#   
#   observe({
#     if (input$submit_data)
#       hide("dia_plot_ph")
#   })
#   
#   ## when main object created, plot cis
#   output$ci_plot <- renderPlot({
#     input$alpha # invalidate when alpha is changed (for immidiate reactivity)
#     o <- get_comparisons_object()
#     if(!is.null(o))
#     {
#       plot_confints(ci_obj_ua = o$co$ci,ci_obj_adj = o$co$ci_new, xlab = o$measure_details$parameter_name)
# #      plot_h <<- nrow(o$tbl_res)
#     }
#       
#     
#     },res = 85,bg = "white"
# #    ,height = function() {session$clientData$output_plot1_width* 0.7 *(1/3)}
#   )
#   
#   
#   outputOptions(output,"ci_plot",suspendWhenHidden = F)
#   
#   observe({
#     if (input$submit_data)
#       show("ci_plot_h")
#   })
#   
#   output$ci_plot_ph <- renderImage({
#     list(src = "WWW/placeholder1.svg",alt = "...")
#   },deleteFile = F)
#   
#   observe({
#     if (input$submit_data)
#       hide("ci_plot_ph")
#   })
# 
#   

  # output$dl_button <- downloadHandler(
  #   filename = function() 
  #   {
  #     if (input$dl_type=="all")
  #       paste0("Results_",input$experiment_identifier,".txt")
  #     else
  #       paste0("Table_",input$experiment_identifier,".csv")
  #   }
  #     ,
  #   content  = function(file)
  #   {
  #     o <- get_comparisons_object()
  #     if (input$dl_type=="all")
  #     {
  #       capture.output(o[-(1:4)], file = file)
  #     } else {
  #       write.csv(o$results_table,file)
  #     }
  #   }
  # )
  # 
  # observe({
  #   if (input$submit_data>0)
  #     removeClass(id = "dl_button", class = "disabled")
  # })
# ---- Examples ----

  ## Example 1:
  
  # observeEvent(input$Example1,{
  #   updateTextInput(session, "lab_name",value = "HMGU")
  #   updateRadioButtons(session,"expr_design",selected = "Tukey")
  #   updateSelectizeInput(session,"genotypes_tested_pairwise",selected = c("Arhgef4","baseline","Elk4","Setmar","Slc38a10","Tnfaip1","Ttll4"))
  #   updateRadioButtons(session,inputId =  "proc_gender",selected = "Females")
  #   updateSelectizeInput(session, "proc_name", selected = "DEXA")
  #   updateNumericInput(session,inputId =  "proc_age", value = "12")
  #   updateNumericInput(session,"proc_duration", value = NA)
  # })
  # 
  # observeEvent(input$proc_name,{
  #   if (input$Example1 > values$Example1_iteration)
  #   {
  #     updateSelectInput(session,inputId = "measure_name",selected = "Fat mass")
  #     updateTextInput(session, inputId = "Arhgef4_mean",value = 0.9322188)
  #     updateTextInput(session, inputId = "baseline_mean",value = 1.3072999)
  #     updateTextInput(session, inputId = "Elk4_mean",value = 1.1512591)
  #     updateTextInput(session, inputId = "Setmar_mean",value = 1.2584906)
  #     # updateTextInput(session, inputId = "Slc38a10_mean",value = 1.4916619)
  #     # updateTextInput(session, inputId = "Tnfaip1_mean",value = 1.9265151)
  #     # updateTextInput(session, inputId = "Ttll4_mean",value = 0.9269960)
  #     updateTextInput(session, inputId = "Arhgef4_SD",value = 0.54814483)
  #     updateTextInput(session, inputId = "baseline_SD",value = 0.53475226)
  #     updateTextInput(session, inputId = "Elk4_SD",value = 0.42346114)
  #     updateTextInput(session, inputId = "Setmar_SD",value = 0.55743598)
  #     # updateTextInput(session, inputId = "Slc38a10_SD",value = 0.51253074)
  #     # updateTextInput(session, inputId = "Tnfaip1_SD",value = 0.55578986)
  #     # updateTextInput(session, inputId = "Ttll4_SD",value = 0.32304317)
  #     updateTextInput(session, inputId = "Arhgef4_N",value = 7)
  #     updateTextInput(session, inputId = "baseline_N",value = 290)
  #     updateTextInput(session, inputId = "Elk4_N",value = 13)
  #     updateTextInput(session, inputId = "Setmar_N",value = 7)
  #     # updateTextInput(session, inputId = "Slc38a10_N",value = 9)
  #     # updateTextInput(session, inputId = "Tnfaip1_N",value = 8)
  #     # updateTextInput(session, inputId = "Ttll4_N",value = 2)
  #     # 
  #     # updateSelectInput(session,inputId = "measure_name",selected = "bolus count")
  #     # updateTextInput(session, inputId = "_mean",value = 0.9322188)
  #     # updateTextInput(session, inputId = "_mean",value = 1.3072999)
  #     # updateTextInput(session, inputId = "_mean",value = 1.1512591)
  #     # updateTextInput(session, inputId = "_mean",value = 1.2584906)
  #     # updateTextInput(session, inputId = "_mean",value = 1.4916619)
  #     # updateTextInput(session, inputId = "_mean",value = 1.9265151)
  #     # updateTextInput(session, inputId = "_mean",value = 0.9269960)
  #     # updateTextInput(session, inputId = "_SD",value = 0.54814483)
  #     # updateTextInput(session, inputId = "_SD",value = 0.53475226)
  #     # updateTextInput(session, inputId = "_SD",value = 0.42346114)
  #     # updateTextInput(session, inputId = "_SD",value = 0.55743598)
  #     # updateTextInput(session, inputId = "_SD",value = 0.51253074)
  #     # updateTextInput(session, inputId = "_SD",value = 0.55578986)
  #     # updateTextInput(session, inputId = "_SD",value = 0.32304317)
  #     # updateTextInput(session, inputId = "_N",value = 7)
  #     # updateTextInput(session, inputId = "_N",value = 290)
  #     # updateTextInput(session, inputId = "_N",value = 13)
  #     # updateTextInput(session, inputId = "_N",value = 7)
  #     # updateTextInput(session, inputId = "_N",value = 9)
  #     # updateTextInput(session, inputId = "_N",value = 8)
  #     # updateTextInput(session, inputId = "_N",value = 2)
  #     # 
  #     
  #     updateCheckboxInput(session,"mult_correct",value = T)
  #     
  #     values$Example1_iteration <- input$Example1
  #   }
  # })
  
  
    
  # ## Example 1:
  # 
  # observeEvent(input$Example1,{
  #   updateTextInput(session, "lab_name",value = "Exmpl1")
  #   updateRadioButtons(session,"expr_design",selected = "Tukey")
  #   updateSelectizeInput(session,"genotypes_tested_pairwise",selected = c("g1","g2","g3","g4"))
  #   updateRadioButtons(session,inputId =  "proc_gender",selected = "Females")
  #   updateSelectizeInput(session, "proc_name", selected = "Open Field Test")
  #   updateNumericInput(session,inputId =  "proc_age", value = "12")
  #   updateNumericInput(session,"proc_duration", value = 10)
  # })
  # 
  # observeEvent(input$proc_name,{
  #   if (input$Example1 > values$Example1_iteration)
  #   {
  #     updateSelectInput(session,inputId = "measure_name",selected = "bolus count")
  #     updateTextInput(session, inputId = "g1_mean",value = 0.84)
  #     updateTextInput(session, inputId = "g2_mean",value = 0.19)
  #     updateTextInput(session, inputId = "g3_mean",value = 1.52)
  #     updateTextInput(session, inputId = "g4_mean",value = 1.27)
  #     
  #     updateTextInput(session, inputId = "g1_SD",value = 0.38)
  #     updateTextInput(session, inputId = "g2_SD",value = 0.30)
  #     updateTextInput(session, inputId = "g3_SD",value = 0.37)
  #     updateTextInput(session, inputId = "g4_SD",value = 0.35)
  #     
  #     updateTextInput(session, inputId = "g1_N",value = 10)
  #     updateTextInput(session, inputId = "g2_N",value = 16)
  #     updateTextInput(session, inputId = "g3_N",value = 12)
  #     updateTextInput(session, inputId = "g4_N",value = 12)
  #     
  #     updateCheckboxInput(session,"mult_correct",value = T)
  #     
  #     values$Example1_iteration <- input$Example1
  #   }
  # })
  
#   ## Example 2:
#   
# #   output$ctrl <- renderText({
# #     input$gctrl_mean
# #   })
#   
#   observeEvent(input$Example2,{
#     updateTextInput(session, "lab_name",value = "Exmpl2")
#     updateRadioButtons(session,"expr_design",selected = "Dunnet")  
#     updateRadioButtons(session,inputId =  "proc_gender",selected = "Males")
#     updateSelectizeInput(session, "proc_name", selected = "Grip-Strength")
#     updateNumericInput(session,inputId =  "proc_age", value = "10")
#     updateNumericInput(session,"proc_duration", value = 20)
#   })
# 
#   observeEvent(input$proc_name,{
#     if (input$Example2 > values$Example2_iteration)
#       updateSelectInput(session,inputId = "measure_name",selected = "Forelimb grip strength normalised against body weight")
#   })
#   
#   observeEvent(input$expr_design,{
#     if (input$Example2 > values$Example2_iteration)
#       updateSelectizeInput(session,"genotypes_tested_control",selected = "gctrl")
#   })
#       
#   observeEvent(input$genotypes_tested_control,{
#     if (input$Example2 > values$Example2_iteration)
#        updateSelectizeInput(session,"genotypes_tested_cases",selected = c("g1","g2","g3"))
#   })
# 
#   observeEvent(input$genotypes_tested_cases,{
#     if (input$Example2 > values$Example2_iteration)
#     {
#       updateTextInput(session, inputId = "g1_mean",value = 1.84)
#       updateTextInput(session, inputId = "g2_mean",value = 1.99)
#       updateTextInput(session, inputId = "g3_mean",value = 2.55)
#       
#       updateTextInput(session, inputId = "g1_SD",value = 0.42)
#       updateTextInput(session, inputId = "g2_SD",value = 0.18)
#       updateTextInput(session, inputId = "g3_SD",value = 0.37)
#       
#       updateTextInput(session, inputId = "g1_N",value = 16)
#       updateTextInput(session, inputId = "g2_N",value = 16)
#       updateTextInput(session, inputId = "g3_N",value = 16)
#     }
#   })
#   
#   observeEvent(input$g1_mean,{
#     if (input$Example2 > values$Example2_iteration)
#     {
#       Sys.sleep(1)
#       updateTextInput(session, inputId = "gctrl_mean",value = 2.27)
#       updateTextInput(session, inputId = "gctrl_SD",value = 0.45)
#       updateTextInput(session, inputId = "gctrl_N",value = 12)
#       values$Example2_iteration <- input$Example2
#     }
#   })
  ## when experiment is selected update values expr_design_syn and genotypes_tested
  # output$genotype_selection_block <- renderUI({
  #   if (input$expr_design=='Tukey')
  #   {
  #     values$expr_design_syn <- "Pairwise"
  #     out <- div(selectizeInput("genotypes_tested_pairwise",'Genotypes tested:', choices = genotypes_vec, multiple = TRUE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }'))))
  #   }
  #     
  #   if (input$expr_design=='Dunnet')
  #   {
  #     values$expr_design_syn <- "CtrlCases"
  #     out <- div(
  #       selectizeInput("genotypes_tested_control",'Genotype tested (control group):', choices = genotypes_vec, multiple = FALSE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list one',onInitialize = I('function() { this.setValue(""); }'))),
  #       selectizeInput("genotypes_tested_cases",'Genotypes tested (cases groups):', choices = genotypes_vec, multiple = TRUE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
  #     )
  #   }
  #   return(out)
  # })
  
  # observe({
  #   if (input$expr_design=='Tukey')
  #   {
  #     values$genotypes_tested <- input$genotypes_tested_pairwise
  #   }
  #   if (input$expr_design=='Dunnet')
  #   {
  #     values$genotypes_tested <- c(input$genotypes_tested_control,input$genotypes_tested_cases)
  #   }
  # })
  # 
  
  ## when control is selected updated the choises for cases:
  # observeEvent(input$genotypes_tested_control,{
  #   control_selected <- input$genotypes_tested_control
  #     updateSelectizeInput(session,inputId = "genotypes_tested_cases", choices = genotypes_vec[genotypes_vec!=control_selected], options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
  # })
  # 
  ## when inputs in step 1 are changed, update the experiment identifier
  # observeEvent({input$lab_name; values$genotypes_tested; input$proc_gender; values$expr_design_syn},{
  #   
  #   lab_name_init <- gsub('([[:punct:]])|\\s+','_',input$lab_name)
  #   genotypes_tested_init <- paste(substr(values$genotypes_tested,1,1),collapse = ".")
  #   proc_gender_init <- switch(input$proc_gender,
  #          "Males" = "Males",
  #          "Females" = "Females",
  #          "Males & Females" = "Both")
  #   expr_design_init <-  values$expr_design_syn
  #   
  #   name <- paste(lab_name_init,
  #                 expr_design_init,
  #                 genotypes_tested_init,
  #                 proc_gender_init,
  #                 humanDate(),sep = "_")
  #   
  #   if (is.null(values$genotypes_tested) || values$genotypes_tested=="")
  #     reset("experiment_identifier")
  #   else
  #     updateTextInput(session,"experiment_identifier",value = name)
  # })
  ## i) update meta data editor
  # tbl_meta_selected <- tbl_meta %>%
  #   filter(procedure_name == proc_name_selected, parameter_name != "Standard Operating Procedure") %>%
  #   select(  Parameter = parameter_name, Value = default ,Units = parameter_unit) %>% 
  #   bind_rows(data.frame("Parameter" ="=========","Value" = "=====" ,Units = "====="),.) %>%  
  #   as.data.frame()
  # 
  # tbl_meta_selected_txt <- paste0(capture.output(print(tbl_meta_selected,right = F, row.names = FALSE)),collapse="\n")
  # 
  # if (nrow(tbl_meta_selected)==0)
  #   updateAceEditor(session,"meta_data_editor",value = ".")
  # else
  #   updateAceEditor(session,"meta_data_editor",value = tbl_meta_selected_txt)
  # 
  ## ii) update measures table
  
  
  
}