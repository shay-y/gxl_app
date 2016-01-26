humanDate <- function() format(Sys.time(), "%Y_%m_%d")

shinyServer(function(input, output, session) {
  
  values <- reactiveValues(Example1_iteration = 0, Example2_iteration = 0, genotypes_tested = NULL,expr_design_syn = NULL)
  
  # observe(label="console",{
  #   if(input$console != 0) {
  #     options(browserNLdisabled=TRUE)
  #     saved_console<-".RDuetConsole"
  #     if (file.exists(saved_console)) load(saved_console)
  #     isolate(browser())
  #     save(file=saved_console,list=ls(environment()))
  #   }
  # })
  
#---- input manipulations:----  
  
  ## when experiment is selected update values expr_design_syn and genotypes_tested
  output$genotype_selection_block <- renderUI({
    if (input$expr_design=='Tukey')
    {
      values$expr_design_syn <- "Pairwise"
      out <- div(selectizeInput("genotypes_tested_pairwise",'Genotypes tested:', choices = genotypes_vec, multiple = TRUE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }'))))
    }
      
    if (input$expr_design=='Dunnet')
    {
      values$expr_design_syn <- "CtrlCases"
      out <- div(
        selectizeInput("genotypes_tested_control",'Genotype tested (control group):', choices = genotypes_vec, multiple = FALSE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list one',onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput("genotypes_tested_cases",'Genotypes tested (cases groups):', choices = genotypes_vec, multiple = TRUE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
      )
    }
    return(out)
  })
  
  observe({
    if (input$expr_design=='Tukey')
    {
      values$genotypes_tested <- input$genotypes_tested_pairwise
    }
    if (input$expr_design=='Dunnet')
    {
      values$genotypes_tested <- c(input$genotypes_tested_control,input$genotypes_tested_cases)
    }
  })
 
  
  ## when control is selected updated the choises for cases:
  observeEvent(input$genotypes_tested_control,{
    control_selected <- input$genotypes_tested_control
      updateSelectizeInput(session,inputId = "genotypes_tested_cases", choices = genotypes_vec[genotypes_vec!=control_selected], options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
  })
  
  ## when inputs in step 1 are changed, update the experiment identifier
  observeEvent({input$lab_name; values$genotypes_tested; input$proc_gender; values$expr_design_syn},{
    
    lab_name_init <- gsub('([[:punct:]])|\\s+','_',input$lab_name)
    genotypes_tested_init <- paste(substr(values$genotypes_tested,1,1),collapse = ".")
    proc_gender_init <- switch(input$proc_gender,
           "Males" = "Males",
           "Females" = "Females",
           "Males & Females" = "Both")
    expr_design_init <-  values$expr_design_syn
    
    name <- paste(lab_name_init,
                  expr_design_init,
                  genotypes_tested_init,
                  proc_gender_init,
                  humanDate(),sep = "_")
    
    if (is.null(values$genotypes_tested) || values$genotypes_tested=="")
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
      paste0("<strong><a href='",SOP_selected,"' target='_blank'>Link to Official Protocol</a></strong>")
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
      if (is.null(values$genotypes_tested))
        return ()
      else
      {
        html <- "<table><thead><tr><th>Group</th><th>Mean</th><th>SD</th><th>N</th></tr></thead><tbody>"
        for (g in values$genotypes_tested)
          html <- paste0(html,"<tr><td>",g,"</td><td><input class='tb' id='",g,"_mean' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_SD' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_N' type='number' value='' step='1'/></td></tr>")
        html <- paste0(html,"</tbody></table>")
        return(list(tbl_html=HTML(html)))
      }
    }
    
    if (expr_design=='Dunnet')
    {
      if (is.null(values$genotypes_tested))
        return ()
      else
      {
        html <- "<table><thead><tr><th>Group</th><th>Mean</th><th>SD</th><th>N</th></tr></thead><tbody>"
        g <- values$genotypes_tested[1]
        html <- paste0(html,"<tr><td>",g,"</td><td><input class='tb' id='",g,"_mean' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_SD' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_N' type='number' value='' step='1'/></td></tr>")
        html <- paste0(html,"<tr><td colspan='4'><hr></td></tr>")
        for (g in values$genotypes_tested[-1])
          html <- paste0(html,"<tr><td>",g,"</td><td><input class='tb' id='",g,"_mean' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_SD' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,"_N' type='number' value='' step='1'/></td></tr>")
        html <- paste0(html,"</tbody></table>")
        return(list(tbl_html=HTML(html)))
      }
    }
  })
  
  output$stats_tbl <- renderUI(create_stats_tbl()$tbl_html)
  
  outputOptions(output, "stats_tbl", suspendWhenHidden = FALSE)
  
  
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
      format(digits=5,nsmall=3)
    parameter_trans_symbol <- selected_measure_details()$parameter_trans_symbol %>% 
      as.character()
    parameter_unit <- selected_measure_details()$parameter_unit %>% 
      as.character()
    withMathJax(HTML("\\(S^2_{int.}/S^2_{error}=\\) ",S2_ratio,
                     "<br><strong>Units:</strong> ",parameter_unit,
                     "<br><strong>Transformation:</strong> ",parameter_trans_symbol,
                     "<p></p>"))
  })
  
# ---- reset operations ----

  observeEvent(input$reset_experiment, {
    reset("lab_name")
    reset("proc_gender") 
    updateSelectizeInput(session,"genotypes_tested_pairwise", options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session,"genotypes_tested_control", options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list one',onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session,"genotypes_tested_cases", options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
    reset("expr_design")
    
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
    measure_details <- selected_measure_details()
    
    genotypes_tested <- values$genotypes_tested
    expr_design <- input$expr_design
    genotypes_tested <- values$genotypes_tested
    S2_int <- measure_details$S2_interaction
    n_lab <- measure_details$n_lab
    n_genotype <- measure_details$n_genotype
    
    type <- ifelse(input$mult_correct,"single-step","none")
    alpha <- input$alpha

    # read input to df
    ids_vec <- c(paste0("input$'",genotypes_tested,"_mean'"),
                 paste0("input$'",genotypes_tested,"_SD'"),
                 paste0("input$'",genotypes_tested,"_N'"))
    
    stats_vec <- unlist(sapply(ids_vec, function(x)eval(parse(text=x))))
    if (is.null(stats_vec))
      return(NULL)
    stats_matrix <- matrix(stats_vec,ncol = 3)
    colnames(stats_matrix) <- c("mean","SD","N")
    rownames(stats_matrix) <- genotypes_tested
      
    # create comparisons object
    co <- gxl_adjust(type = type, name_vec = genotypes_tested,
                     mean_vec = stats_matrix[,1], SD_vec = stats_matrix[,2], N_vec = stats_matrix[,3],
                     design = expr_design, S2_int, n_lab, n_genotype, alpha = alpha)
    tbl_res <- get_res_table(co)
    
    measure_list <- measure_details[c("Gender","parameter_id","procedure_name","parameter_name","S2_interaction","parameter_trans_symbol","parameter_unit","S2_ratio")]
    details <- sapply(measure_list,as.character)
    
    return(list(
      measure_details=measure_details,
      tbl_res=tbl_res,
      co=co,
      stats_matrix=stats_matrix,
      genotypes_tested = genotypes_tested,
      expr_design = expr_design,
      alpha=alpha,
      input_stats_matrix = stats_matrix,
      details = details,
      results_table=tbl_res))
  },ignoreNULL = F)
  
  ## when main object created, use it to print table
  output$out_tbl <- DT::renderDataTable({
    input$alpha # invalidate when alpha is changed (for immidiate reactivity)
    o <- get_comparisons_object()
    if (is.null(o))
    {
      tbl_NAs <- data.frame(as.list(rep(NA,7)))
      colnames(tbl_NAs) <- c("p_value","p_value_adj", "est", "ci_lwr", "ci_upr",  "ci_lwr_adj", "ci_upr_adj")
      rownames(tbl_NAs) <- "-"
      get_datatable(tbl_NAs,NULL)
    }else{
      get_datatable(o$tbl_res,o$measure_details$parameter_name)
    }
  })
  
  outputOptions(output,"out_tbl",suspendWhenHidden = F)
  
  ## when main object created, plot diagram
  observe({
    o <- get_comparisons_object()
    if(!is.null(o))
    {
      bind_shiny(vis = reactive({
        
        # load("o.Rdata")
        
        
        tbl_res <- o$tbl_res[,1:3]
        pair_names <- matrix(unlist((strsplit(rownames(tbl_res)," - "))),ncol=2,byrow = T)
        tbl_pairs <- data.frame(name1 = pair_names[,1],name2 = pair_names[,2],tbl_res)
        tbl_singles <- data.frame(name=o$genotypes_tested,mean=o$stats_matrix[,1])
        plot_diagram(tbl_singles,tbl_pairs,alpha = 0.05, xMeasure = as.character(o$measure_details$parameter_name))
      }), plot_id = "dia_plot",session = session, ,bg = "white", ,background = "white")
    }
  })
  

  observe({
    if (input$submit_data)
      show("dia_plot_h")
  })
  
  output$dia_plot_ph <- renderImage({
    list(src = "WWW/placeholder1.svg",alt = "...")
  },deleteFile = F)
  
  observe({
    if (input$submit_data)
      hide("dia_plot_ph")
  })
  
  ## when main object created, plot cis
  output$ci_plot <- renderPlot({
    input$alpha # invalidate when alpha is changed (for immidiate reactivity)
    o <- get_comparisons_object()
    if(!is.null(o))
    {
      plot_confints(ci_obj_ua = o$co$ci,ci_obj_adj = o$co$ci_new, xlab = o$measure_details$parameter_name)
#      plot_h <<- nrow(o$tbl_res)
    }
      
    
    },res = 85,bg = "white"
#    ,height = function() {session$clientData$output_plot1_width* 0.7 *(1/3)}
  )
  
  
  outputOptions(output,"ci_plot",suspendWhenHidden = F)
  
  observe({
    if (input$submit_data)
      show("ci_plot_h")
  })
  
  output$ci_plot_ph <- renderImage({
    list(src = "WWW/placeholder1.svg",alt = "...")
  },deleteFile = F)
  
  observe({
    if (input$submit_data)
      hide("ci_plot_ph")
  })

#   observe(label="console",{
#     if(input$console != 0) {
#       options(browserNLdisabled=TRUE)
#       saved_console<-".RDuetConsole"
#       if (file.exists(saved_console)) load(saved_console)
#       isolate(browser())
#       save(file=saved_console,list=ls(environment()))
#     }
#   })

  output$dl_button <- downloadHandler(
    filename = function() 
    {
      if (input$dl_type=="all")
        paste0("Results_",input$experiment_identifier,".txt")
      else
        paste0("Table_",input$experiment_identifier,".csv")
    }
      ,
    content  = function(file)
    {
      o <- get_comparisons_object()
      if (input$dl_type=="all")
      {
        capture.output(o[-(1:4)], file = file)
      } else {
        write.csv(o$results_table,file)
      }
    }
  )
  
  observe({
    if (input$submit_data>0)
      removeClass(id = "dl_button", class = "disabled")
  })
# ---- Examples ----
  
  ## Example 1:
  
  observeEvent(input$Example1,{
    updateTextInput(session, "lab_name",value = "Exmpl1")
    updateRadioButtons(session,"expr_design",selected = "Tukey")
    updateSelectizeInput(session,"genotypes_tested_pairwise",selected = c("g1","g2","g3","g4"))
    updateRadioButtons(session,inputId =  "proc_gender",selected = "Females")
    updateSelectizeInput(session, "proc_name", selected = "Open Field Test")
    updateNumericInput(session,inputId =  "proc_age", value = "12")
    updateNumericInput(session,"proc_duration", value = 10)
  })
  
  observeEvent(input$proc_name,{
    if (input$Example1 > values$Example1_iteration)
    {
      updateSelectInput(session,inputId = "measure_name",selected = "bolus count")
      updateTextInput(session, inputId = "g1_mean",value = 0.84)
      updateTextInput(session, inputId = "g2_mean",value = 0.19)
      updateTextInput(session, inputId = "g3_mean",value = 1.52)
      updateTextInput(session, inputId = "g4_mean",value = 1.27)
      
      updateTextInput(session, inputId = "g1_SD",value = 0.38)
      updateTextInput(session, inputId = "g2_SD",value = 0.30)
      updateTextInput(session, inputId = "g3_SD",value = 0.37)
      updateTextInput(session, inputId = "g4_SD",value = 0.35)
      
      updateTextInput(session, inputId = "g1_N",value = 10)
      updateTextInput(session, inputId = "g2_N",value = 16)
      updateTextInput(session, inputId = "g3_N",value = 12)
      updateTextInput(session, inputId = "g4_N",value = 12)
      
      updateCheckboxInput(session,"mult_correct",value = T)
      
      values$Example1_iteration <- input$Example1
    }
  })
  
  ## Example 2:
  
#   output$ctrl <- renderText({
#     input$gctrl_mean
#   })
  
  observeEvent(input$Example2,{
    updateTextInput(session, "lab_name",value = "Exmpl2")
    updateRadioButtons(session,"expr_design",selected = "Dunnet")  
    updateRadioButtons(session,inputId =  "proc_gender",selected = "Males")
    updateSelectizeInput(session, "proc_name", selected = "Grip-Strength")
    updateNumericInput(session,inputId =  "proc_age", value = "10")
    updateNumericInput(session,"proc_duration", value = 20)
  })

  observeEvent(input$proc_name,{
    if (input$Example2 > values$Example2_iteration)
      updateSelectInput(session,inputId = "measure_name",selected = "Forelimb grip strength normalised against body weight")
  })
  
  observeEvent(input$expr_design,{
    if (input$Example2 > values$Example2_iteration)
      updateSelectizeInput(session,"genotypes_tested_control",selected = "gctrl")
  })
      
  observeEvent(input$genotypes_tested_control,{
    if (input$Example2 > values$Example2_iteration)
       updateSelectizeInput(session,"genotypes_tested_cases",selected = c("g1","g2","g3"))
  })

  observeEvent(input$genotypes_tested_cases,{
    if (input$Example2 > values$Example2_iteration)
    {
      updateTextInput(session, inputId = "g1_mean",value = 1.84)
      updateTextInput(session, inputId = "g2_mean",value = 1.99)
      updateTextInput(session, inputId = "g3_mean",value = 2.55)
      
      updateTextInput(session, inputId = "g1_SD",value = 0.42)
      updateTextInput(session, inputId = "g2_SD",value = 0.18)
      updateTextInput(session, inputId = "g3_SD",value = 0.37)
      
      updateTextInput(session, inputId = "g1_N",value = 16)
      updateTextInput(session, inputId = "g2_N",value = 16)
      updateTextInput(session, inputId = "g3_N",value = 16)
    }
  })
  
  observeEvent(input$g1_mean,{
    if (input$Example2 > values$Example2_iteration)
    {
      Sys.sleep(1)
      updateTextInput(session, inputId = "gctrl_mean",value = 2.27)
      updateTextInput(session, inputId = "gctrl_SD",value = 0.45)
      updateTextInput(session, inputId = "gctrl_N",value = 12)
      values$Example2_iteration <- input$Example2
    }
  })
})