function(input, output, session) {
  
  ## create temporary debugging concole: ----
  # observe(label="console",{
  #   if(input$console != 0) {
  #     options(browserNLdisabled=TRUE)
  #     saved_console<-".RDuetConsole"
  #     if (file.exists(saved_console)) load(saved_console)
  #     isolate(browser())
  #     save(file=saved_console,list=ls(environment()))
  #   }
  # })
  
  # runcodeServer()  
  
  ## initialize reactive values: ----
  values <- reactiveValues(
    examples_completed = 0,
    example_step = 0,
    # example_file_loaded = F,
    file = NULL
    )

  ## create metadata and measure inputs table for the selected procedure: ----
  output$metadata_input <- renderUI({
    req(input$procedure_name)
    
    selected_url <- 
      tbl_procedures %>% 
      filter(procedure_name == input$procedure_name) %>%
      select(url) %>% 
      .$url
    
    tbl_metadata_selected <- tbl_metadata %>% 
      filter(procedure_name == input$procedure_name)
    
    measure_name_list <-
      tbl_models %>%
      filter(procedure_name == input$procedure_name) %>% 
      .$parameter_name %>%
      as.character() %>%
      as.list() %>%
      {setNames(.,.)}
    
    tags$table(
      id = "table-metadata",
      class = "table table-bordered table-gxl",
      tags$caption(
        tags$b("Parameters for the ",tags$i(input$procedure_name)," procedure:"),
        br(),
        "(See extensive information about the procedure in its Standard Operating Procedure document on ",
        a(href = selected_url,target='_blank',"IMPReSS"),
        " site)"
      ),        
      tags$thead(
        tags$tr(
          tags$th("Parameter name"),tags$th("Value"),tags$th("Unit")
        )
      ),
      tags$tbody(
        apply(
          tbl_metadata_selected,1,
          function(row)
          {
            row <- as.list(row)
            tags$tr(
              tags$td(row$parameter_name),
              tags$td(
                {
                  if (row$datatype == "TEXT")
                  {
                    selectizeInput(
                      inputId = row$parameter_name,
                      label = NULL,
                      selected = row$default_value,
                      choices = {row$input_options %>% parse(text = .) %>% eval() %>% {setNames(.,.)}},
                      options = list(create = TRUE))
                  } else
                    if (row$datatype == "INT")
                    {
                      numericInput(
                        inputId = row$parameter_name,
                        label = NULL,
                        step = 1,
                        value = row$default_value,
                        min = row$input_min,
                        max = row$input_max)
                    } else
                      if (row$datatype == "FLOAT")
                      {
                        numericInput(
                          inputId = row$parameter_name,
                          label = NULL,
                          value = row$default_value,
                          min = row$input_min,
                          max = row$input_max)
                      }
                }
              ),
              tags$td(row$unit)
            )
          }
        ),
        tags$tr(
          tags$th(
            span(
              class='step',
              ""),
            "phenotypic measure"),
          tags$td(
            selectizeInput(
              inputId = "measure_selected",
              label = NULL,
              choices = measure_name_list,
              options = list(create = TRUE))),
          tags$td(uiOutput("unit"))
        ),
        hidden(
          tags$tr(id = "duration_line",
            tags$td(
              "Duration"
            ),
            tags$td(
              numericInput(
                inputId = "duration",
                label = NULL,
                min = 0,
                max = 60,
                value = 5
              )
            ),
            tags$td(
              "minutes"
            )
          )
        )
      )
    )
  })
  
  output$unit <- renderUI({
    req(input$measure_selected,input$procedure_name)
    tbl_models %>%
      filter(procedure_name == input$procedure_name,
             parameter_name == input$measure_selected) %>% 
      distinct(unit) %>% .$unit
  })
  
  ## if measure has duration (it is series) then add input for the duration:----
  observe({
    toggle(id = "duration_line",anim = F, condition = !is.null(tbl_matched_model()) & nrow(tbl_matched_model())>0 & all( !is.na(tbl_matched_model()$duration) ))
  })
  
  ## filter the models table to get only rows that match the user input (sex, procedure, measure) and check refinment conditions:----
  # tbl with 0 rows represents no matchs
  tbl_matched_model <- reactive(
    {
      req(input$measure_selected,input$procedure_name)
      tbl_matched_models <- tbl_models %>%
        filter(procedure_name == input$procedure_name,
               parameter_name == input$measure_selected,
               sex            == input$Sex) %>% 
        filter(metadata_rules %>% parse(text = .) %>% eval())
      
      ## estimate gxl ratio:
      if ( nrow(tbl_matched_models)>0 & all( !is.na(tbl_matched_models$duration) ) )
      {
        x_pred <- input$duration
        x <- tbl_matched_models$duration %>% as.numeric()
        y <- tbl_matched_models$s2_ratio  
        y_pred <- predict.lm(lm(y~x,data = data.frame(y,x)), newdata = data.frame(x = x_pred))
        tbl_matched_model <- tbl_matched_models %>% 
          mutate(duration = x_pred, s2_ratio =  y_pred) %>% 
          select(-s2_interaction, s2_lab, s2_error) %>% 
          distinct()
      }
      else
        tbl_matched_model_ <- tbl_matched_models
      
      if(nrow(tbl_matched_model_)>1) {cat("Too many matchs.")}

      # if(nrow(tbl_matched_model_)==0) {
      #   cat("No match found. analysing only unadjusted comparisons")
      #   tbl_matched_model_ %>% 
      #     add_row()
      #   }
      
      tbl_matched_model_
    }
  )

  ## render details table for selected measure: ----
  output$measure_selected_details <- renderUI({
    req(tbl_matched_model())
    withTags(
      if (nrow(tbl_matched_model())>0)
      {
        ul(
          li(
            b("GxL replicability ratio estimate: "),
            withMathJax("\\(S^2_{G\\times L}/S^2_{error}=\\)"),
            tbl_matched_model()$s2_ratio %>% signif(digits=6)
          ),
          li(
            if (tbl_matched_model()$transformation_symbol != "none")
              list(b("Transformation taken prior to analysis: "), tbl_matched_model()$transformation_symbol) #
            else
              b("No transformation is taken prior to analysis")
          )
        )
      } else
      {
        ul(
          li(
            b("GxL replicability ratio estimate not found")
          ),
          li(
            b("No transformation is taken prior to analysis")
          )
        )
      }
    )
  })
  
  ## create input object to receive raw data file: ----
  output$file_form <- renderUI({
    req(input$input_method=="file")
    
    # take dependencies on previous selections and reset button
    input$procedure_name
    input$measure_selected
    input$input_method
    
    
    # if(values$example_file_loaded)
    #   div(
    #     actionButton(inputId = "reset_example",label = "Unload",class = "btn_right btn-sm"),
    #     "Example data input is loaded."
    #   )
    # else
    # {
      div(
        # actionButton(inputId = "reset_upload",label = "Reset", icon = icon("refresh"),class = "btn_right btn-sm"),
        downloadButton(outputId = "example_raw_input",label = "Download example file", class = "btn_right btn-sm"),
        div(
          class="form-group shiny-input-container",
          style="width: 70%;",
          #<label></label>
          tags$input(
            id="upload_file",
            name="upload_file",
            type="file",
            accept="text/csv"),
          div(
            id="upload_file_progress",
            class="progress progress-striped active shiny-file-input-progress",
            div(class="progress-bar"
            )
          )
        )
      )
   # }
  })
  
  ## pass file input to value to workaround resetting file data----
  observe({
    values$file <- input$upload_file
  })
  
  # observe({
  #   input$reset_example
  #   values$example_file_loaded <- F
  # })
  
  ## reset the following: file value (file input element resets above) or group summaries inputs : ----
  ## on procedure, meassure, reset, input method switch events
  observe({
    #take dependencies on previous selections
    input$procedure_name
    input$measure_selected
    input$input_method
    # if (!values$example_file_loaded)
    values$file <- NULL
    reset(id = "table-groups")
    disable("submit")
  })
  
  ## link the example raw data file:  ----
  output$example_raw_input <- downloadHandler(
    filename = "gxl_app_example_input.csv",
    content = function(con) {
      write.table(x = tbl_example_raw_data,file = con,row.names = F,col.names = F, qmethod = "double",sep = ",")
    }
  )
  ## create input form for groups selection: ----
  output$groups_form <- renderUI({
    req(input$input_method=="summ")
    selectizeInput(
      inputId = "groups",
      label = "Groups names:",
      choices = group_names_list,
      multiple = T,
      options = list(create = TRUE))
  })
  
  ## read raw data from file: ----
  tbl_raw_data <- eventReactive(eventExpr = values$file,
    {
      req(input$input_method=="file",values$file)
      if(is.character(values$file) & values$file == "example")
        tbl_example_raw_data 
      else
        read.csv(
          file = values$file$datapath,
          header = F,
          stringsAsFactors = F
        )
    })  
  
  #observe(print(values$file))
  observe(print(req(input$input_method)))
  observe(print(req(input$groups)))
  
  
  ## summarize and transform raw table: ----
  file_summaries <- reactive(
    {
      req(tbl_raw_data(),tbl_matched_model())
      enable("submit")
      if (nrow(tbl_matched_model())==0)
        trans_fun <- eval(parse(text=paste("function(x) x")))
      else
        trans_fun <- eval(parse(text=paste("function(x)",tbl_matched_model()$transformation_expr)))
      
      tbl_raw_data()  %>% 
        mutate(group_name = as.factor(V1), transformed = trans_fun(V2)) %>% 
        group_by(group_name) %>% 
        summarise(mean.t = mean(transformed,na.rm = T),
                  mean = mean(V2,na.rm = T),
                  sd.t =sd(transformed,na.rm = T),
                  sd =sd(V2,na.rm = T),
                  n =sum(!is.na(transformed))) 
    })
    
  ## render file summaries table: ----
  
  output$file_summaries <- renderDataTable(
    {
      req(file_summaries())
      datatable(
        caption = "Groups summaries",
        data =  file_summaries() %>% select(Group = group_name, Mean = mean.t,`Standard Deviation`	= sd.t,`Num. of Observations` =	n)
      ) %>% formatSignif(2:3,digits = 5)
    }
  )
  
  ## render input summaries table: ----
  
  output$groups_summaries <- renderUI(
    {
      req(input$groups)
      tags$table(
        id = "table-groups",
        class = "table table-gxl",
        tags$thead(
          tags$tr(
            tags$th("Group name"),tags$th("Mean"),tags$th("Standard Deviation"),tags$th("N (# Observations)")
          )
        ),
        tags$tbody(
          lapply(
            1:length(input$groups),function(g)
            {
              tags$tr(
                tags$td(input$groups[g]),
                tags$td(numericInput(inputId = paste("grp",g,input$groups[g],"mean.t",sep = "_"), label = NULL, value = "")),
                tags$td(numericInput(inputId = paste("grp",g,input$groups[g],"sd.t",sep = "_"), label = NULL, value = "", min = 0)),
                tags$td(numericInput(inputId = paste("grp",g,input$groups[g],"n",sep = "_"), label = NULL, value = "", step = 1,min = 0))
              )
            }
          )
        )
      )
    }
  )
  
  ## render group info table: ----
  
  output$groups_info <- renderUI(
    {
      if (input$input_method=="file")
        groups <- req(file_summaries()$group_name)
      if (input$input_method=="summ")
        groups <- req(input$groups)
      
      # popify(
      #   el = icon("info-circle"),
      #   title = NULL,
      #   content = withTags(tagList(
      #     
      #   ))%>%  str_replace_all(pattern = "\n",replacement = ""),
      #   placement = "right",
      #   trigger = c("hover","focus"),
      #   options = NULL)
      withTags(
        tagList(
          br(),
          "Please fill-in additional details for each experimental group.",
          ul(
            li("For ",b("Background strain")," and ",b("Genetic manipulation")," you may use the nomeclature as in ",
               a(href="http://www.findmice.org/","International Mouse Strain Resource",target="_blank"),"."),
            li("You may either specify the treatments or use obsfucated names ('Treatment1','Treatment2', etc.).")
          ),
          table(
            id = "table-groups-info",
            class = "table table-gxl",
            thead(
              tr(
                th("Group"),th(
                  "Background strain"
                ),
                th(
                  "Genetic manipulation"
                ),
                th(
                  "Treatment"
                )
              )
            ),
            tbody(
              lapply(
                1:length(groups),function(g)
                {
                  tr(
                    td(groups[g]),
                    td(textInput(inputId = paste("grp_info_",g,groups[g],"bg.strain",sep = "_"), label = NULL)), # choices = {c(" ",bg_strain_vec) %>% {setNames(.,.)}},options = list(create = TRUE))
                    td(textInput(inputId = paste("grp_info_",g,groups[g],"genetic.manipulation", sep = "_"), label = NULL)),  # choices = {c(" ",gene_symbol_vec) %>% {setNames(.,.)}},options = list(create = TRUE)))
                    td(textInput(inputId = paste("grp_info_",g,groups[g],"treatment",sep = "_"), label = NULL)) # choices = {c(" ",treatment_vec) %>% {setNames(.,.)}},options = list(create = TRUE)))  
                  )
                }
              )
            )
          )
        )
      )
    }
  )
  
  ## read from groups summaries input table:----
  
  tbl_group_summaries <- reactive(
    {
      input_grp_names <- names(input) %>% str_extract("grp.*") %>% na.omit() %>% unique()
      req(input_grp_names,input$groups)
      
      tbl_summ <-
        input_grp_names %>%
        str_split_fixed("_",n = 4) %>%
        as_data_frame() %>%
        transmute(
          group_name = V3,
          key        = V4,
          input_grp_names) %>% 
        rowwise() %>% 
        filter(group_name %in% input$groups)
      tbl_summ <- tbl_summ  %>% 
        mutate(
          value = input[[input_grp_names]]) %>% 
        select(-input_grp_names) %>% 
        spread(key = key,value = value)
      return(tbl_summ)
    })
  
  observe({
    req(tbl_group_summaries())
    if (nrow(tbl_group_summaries())>1 &
        all(!is.na(tbl_group_summaries()[,-1])) &
        all(tbl_group_summaries()[,-1]!="") &
        isTruthy(input$measure_selected))
      enable("submit")
  })
  
  
  ## on submit, push data to server: ----
  
  observeEvent(
    eventExpr = {input$submit; values$example_submit},
    handlerExpr = 
    {
      req(input$agree_contribute)
      
      input_copy <- reactiveValuesToList(input)
      if(input$input_method=="file")
        reactives_copy <- list(
          tbl_matched_model(),
          tbl_raw_data(),
          file_summaries())
      else
        reactives_copy <- list(
          tbl_matched_model(),
          tbl_group_summaries())
      user_data <- c(input_copy,reactives_copy,reactiveValuesToList(session$clientData))
      
      sys_time <- Sys.time()
      file_name_rds <- paste0("userdata_",input$email,"_",format(sys_time,'_%Y_%m_%d__%H_%M_%S__%Z.rds'))
      
      saveRDS(object = user_data  ,file = file_name_rds)
      drop_upload(file = file_name_rds,dest = drop_dir,overwrite = F,dtoken = token)
      unlink(file_name_rds)
      
      file_name_txt <- paste0("userdata_",input$email,"_",format(sys_time,'_%Y_%m_%d__%H_%M_%S__%Z.txt'))
      capture.output({
        print(paste0("####--",sys_time,"-",input$email,"----"))
        print(user_data)
        print("####---------------------------")
       },file = file_name_txt)
      drop_upload(file = file_name_txt,dest = drop_dir,overwrite = F)
      unlink(file_name_txt)
  })
  
        


      
   
  ## on submit, copy summaries from the selected input method and add calculations: ----
  tbl_summaries <- eventReactive(
    eventExpr = input$submit,
    valueExpr = 
    {
      
      tbl_summ <- switch(
        input$input_method,
        "file" = req(file_summaries()),
        "summ" = req(tbl_group_summaries())
      )
      
      ## calculate S2_pooled, df and gxl-adjusted df
      tbl_summ <- tbl_summ %>%
        mutate(
          df_pooled = sum(n)-n(),
          s2_pooled = sum(sd.t^2*(n-1))/df_pooled
        ) %>% 
        rownames_to_column(var = "group_id")
      
      tbl_summ_temp <<- tbl_summ
      return(tbl_summ)
    })
  
  ## calculate (estimate, se, statistic ,p-value, conf.lower, conf.higher)X(unadjusted, adjusted GxL) ----
  tbl_pairs <- eventReactive({tbl_summaries(); input$conf_level; input$mult_adjust},
    {
      s2_ratio <- tbl_matched_model()$s2_ratio
      n_labs_s2gxl <- tbl_matched_model()$n_labs_s2gxl
      n_groups_s2gxl <- tbl_matched_model()$n_groups_s2gxl
      alpha <- 1-input$conf_level
      tbl_summ <- tbl_summaries()
      nmeans_tukey <-  nrow(tbl_summ)  
      
      ## generate all pairs combinations,
      ## create pairs table,
      ## calculate stats:
      
      tbl_pairs_ <- 
        nrow(tbl_summ) %>%
        combn(2) %>% t() %>% .[,2:1] %>% as_data_frame() %>% 
        transmute(group_id1 = as.character(V1),group_id2 = as.character(V2)) %>% 
        inner_join(tbl_summ,by = c("group_id1"="group_id")) %>%
        inner_join(tbl_summ,c("group_id2"="group_id","s2_pooled","df_pooled")) %>% 
        transmute(
          pair_id = paste0(group_id1,"-",group_id2),
          name_pair = paste(group_name.x,"-",group_name.y),
          grp1  = group_name.x,
          grp2  = group_name.y,
          mean.t.x,
          mean.t.y,
          sd.t.x,
          sd.t.y,
          n.x,
          n.y,
          s2_pooled,
          df_pooled,
          
          diff  = mean.t.x-mean.t.y,                  
          se    = sqrt(s2_pooled * (1/n.x+1/n.y) ),
          se_gxl= sqrt(s2_pooled * (1/n.x+1/n.y+2*s2_ratio) ),
          stat     = abs(diff) / se,
          stat_gxl = abs(diff) / se_gxl,
          
          df_gxl = ( (1/n.x+1/n.y) * s2_pooled + 2 * s2_ratio * s2_pooled ) ^2   /
            ( ((1/n.x+1/n.y) * s2_pooled)^2/df_pooled + (2 * s2_ratio * s2_pooled)^2 / (n_labs_s2gxl-1) / (n_groups_s2gxl-1) ),
          
          pv      = 2*(1-pt(q = stat    ,df = df_pooled )),
          pv_gxl  = 2*(1-pt(q = stat_gxl,df = df_gxl    )))
      
      if (input$mult_adjust == "Tukey HSD")
        tbl_pairs_ <- tbl_pairs_ %>%  mutate(
          pv      = ptukey(q = sqrt(2) * stat    , nmeans = nmeans_tukey, df = df_pooled, lower.tail = F),
          pv_gxl  = ptukey(q = sqrt(2) * stat_gxl, nmeans = nmeans_tukey, df = df_gxl   , lower.tail = F),
          
          lwr     = diff - qtukey(p = 1-alpha, nmeans = nmeans_tukey, df = df_pooled) * se / sqrt(2),
          upr     = diff + qtukey(p = 1-alpha, nmeans = nmeans_tukey, df = df_pooled) * se / sqrt(2),
          
          lwr_gxl = diff - qtukey(p = 1-alpha, nmeans = nmeans_tukey, df = df_gxl)*se_gxl / sqrt(2),
          upr_gxl = diff + qtukey(p = 1-alpha, nmeans = nmeans_tukey, df = df_gxl)*se_gxl / sqrt(2))
      
      if (input$mult_adjust == "BH selected")
        tbl_pairs_ <- tbl_pairs_ %>%  mutate(
          pv     = p.adjust(pv    ,"BH"),
          pv_gxl = p.adjust(pv_gxl,"BH"),
          
          Q     = max(1, sum(pv     <= alpha)) / n(),
          Q_gxl = max(1, sum(pv_gxl <= alpha)) / n(),
          
          lwr = diff - qt(1-alpha/2*Q ,df = df_pooled)*se %>% as.numeric(),
          upr = diff + qt(1-alpha/2*Q ,df = df_pooled)*se %>% as.numeric(),
          
          lwr_gxl = diff - qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric(),
          upr_gxl = diff + qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric())
      
      if (input$mult_adjust == "none")
        tbl_pairs_ <- tbl_pairs_ %>% mutate(
          lwr = diff - qt(1-alpha/2 ,df = df_pooled)*se %>% as.numeric(),
          upr = diff + qt(1-alpha/2 ,df = df_pooled)*se %>% as.numeric(),
          
          lwr_gxl = diff - qt(1-alpha/2 ,df = df_gxl)*se_gxl %>% as.numeric(),
          upr_gxl = diff + qt(1-alpha/2 ,df = df_gxl)*se_gxl %>% as.numeric())
      
      tbl_pairs_temp <<- tbl_pairs_
      
      return(tbl_pairs_)
    })
  
  tbl_pairs_bt <- reactive(
    {
      req(tbl_matched_model()$transformation_symbol != "x",tbl_pairs())
      back_transform_expr <- tbl_matched_model()$back_transform_expr
      back_trans_fun <- eval(parse(text=paste("function(y)",back_transform_expr)))
      
      tbl_pairs() %>% 
        req() %>% 
        transmute(
          pair_id = pair_id,
          name_pair,
          diff  = back_trans_fun(diff),
          pv    = pv,
          pv_gxl= pv_gxl,
          lwr = back_trans_fun(lwr),
          upr = back_trans_fun(upr),
          lwr_gxl = back_trans_fun(lwr_gxl),
          upr_gxl = back_trans_fun(upr_gxl)
        )
    }
  )
  
  ## render DT1: ----
  output$results_table <- DT::renderDataTable(
    expr = 
    {
      req(tbl_pairs())
      datatable(class = "display compact", # BS: table table-striped table-bordered table-condensed table-hover
        options = 
          list(
            autoWidth = F,
            dom = 't',
            paging = F,
            scrollY = "300px",
            scrollCollapse = T,
            scrollX = TRUE), # fixedColumns = list(leftColumns = 2),
            # buttons = list(
            #   'colvis',
            #   'csv',
            #   'excel',
            #   'print'
            # ) 
        # extensions = 'Buttons',
        data = tbl_pairs() %>% select(name_pair,diff,pv,lwr,upr,pv_gxl,lwr_gxl,upr_gxl),
        rownames = FALSE,
        container =
          withTags(
            table(
              thead(
                tr(
                  th(rowspan = 2, 'Comparison'),
                  th(rowspan = 2, 'Difference'),
                  th(colspan = 3, 'Unadjusted'),
                  th(colspan = 3, 'GxL-Adjusted')
                ),
                tr(
                  th('p-value'),
                  th('CI-Low'),
                  th('CI-High'),
                  th('p-value'),
                  th('CI-Low'),
                  th('CI-High')
                )
              )
            )
          ),
        caption = 
          if(isolate(expr = tbl_matched_model()$transformation_symbol) == "none")
          {
            tags$caption(
              'Pairwise comparisons differences', 
              if (input$mult_adjust == "BH selected") {' (BH adjusted for FDR control):'} else if (input$mult_adjust == "Tukey HSD") {' (Tukey HSD adjusted to control FWER):'} else ':')
          } else 
          {
            tags$caption(
              'Table 1: Pairwise comparisons differences on ',tags$strong('transformed'),' scale'
             # if (input$mult_adjust == "BH selected") {' (BH adjusted for FDR control):'} else if (input$mult_adjust == "Tukey HSD") {' (Tukey HSD adjusted to control FWER):'} else':')
          )}
        ) %>% formatSignif(2:8,digits = 5)
    }
  )
  
  ## render DT2 (back transformed): ----
  output$results_table_bt <- renderDataTable(
    expr = 
    {
      req(tbl_pairs_bt())
      datatable(
        class = "display compact", # BS: table table-striped table-bordered table-condensed table-hover
        options = 
          list(
            autoWidth = F,
            dom = 't',
            paging = F,
            scrollY = "300px",
            scrollCollapse = T,
            scrollX = TRUE),
            # buttons = list(
            #   'colvis',
            #   'csv',
            #   'excel',
            #   'print'
            # ) 
        #extensions = 'Buttons',
        data = tbl_pairs_bt() %>% select(name_pair,diff,pv,lwr,upr,pv_gxl,lwr_gxl,upr_gxl),
        rownames = FALSE,
        container =
          withTags(
            table(
              thead(
                tr(
                  th(rowspan = 2, 'Comparison'),
                  th(rowspan = 2, 'Difference'),
                  th(colspan = 3, 'Unadjusted'),
                  th(colspan = 3, 'GxL-Adjusted')
                ),
                tr(
                  th('p-value'),
                  th('CI-Low'),
                  th('CI-High'),
                  th('p-value'),
                  th('CI-Low'),
                  th('CI-High')
                )
              )
            )
          ),
        caption = 
          tags$caption(
            'Table 2: Pairwise comparisons differences on ',tags$strong('original'),' scale'
            #if (input$fdr_adjust) {' (BH adjusted for selection):'} else ':')
        )) %>% formatSignif(2:8,digits = 5)
      }
  )
  
  ## render pcci plot (back transformed): ----
  output$pcci_plot <- renderPlot(
    {
      req(tbl_pairs())
      plot_pcci(tbl_pairs = tbl_pairs(),
                title = paste("Means Differences Confidence Intervals of",tbl_matched_model()$parameter_name),
                ylab = 
                  if (tbl_matched_model()$transformation_symbol != "none")
                    paste(tbl_matched_model()$parameter_name,"(",tbl_matched_model()$unit,")\nafter",tbl_matched_model()$transformation_symbol,"transformation")
                 else
                  paste(tbl_matched_model()$parameter_name,"(",tbl_matched_model()$unit,")"))
      
    }
  )

  ## render boxplot : ----
  output$box_plot <- renderPlot(
    {
      req(tbl_pairs(),input$input_method=="file")
      if (nrow(tbl_matched_model())==0)
      {
        transformation_symbol <- "none"
        trans_fun <- eval(parse(text=paste("function(x) x")))
        unit <- ""
      }
      else
      {
        transformation_symbol <- tbl_matched_model()$transformation_symbol
        trans_fun <- eval(parse(text=paste("function(x)",tbl_matched_model()$transformation_expr)))
        unit <- tbl_matched_model()$unit
      }
      ggplot(data = tbl_raw_data() %>% req() %>% 
               mutate(group_name = as.factor(V1), measure = trans_fun(V2))
             ) + 
        aes(x = group_name, y = measure) + 
        geom_boxplot() + #width = bw
        ylab(
          if (transformation_symbol != "none")
            paste(input$measure_selected,"(",unit,") after",transformation_symbol,"transformation")
          else
            paste(input$measure_selected,"(",unit,")")
        ) + 
        xlab("")+
        ggtitle(paste("Groups boxplots of",input$measure_selected)) +
        theme_minimal()
    })
    
  ## render boxplot (back transformed): ----
  # output$box_plot_bt <- renderPlot(
  #   {
  #     req(tbl_pairs_bt())
  #     ggplot(data = 
  #              tbl_raw_data() %>% req() %>% 
  #              mutate(group_name = as.factor(V1), measure = V2)
  #     ) + 
  #       aes(x = group_name, y = measure) + 
  #       geom_boxplot() + #width = bw
  #       ylab(
  #         tbl_matched_model()$unit
  #       ) + 
  #       ggtitle(paste("Groups boxplots of",tbl_matched_model()$parameter_name," before transformation")) +
  #       theme_minimal()
  #   })
  
  observe({
    req(file_summaries())
    toggle(
      id = "file_summaries",
      anim = F,
      condition =  !is.null(req(file_summaries())))
  })
    
  
  observe({
    req(tbl_pairs_bt())
    toggle(
      id = "results_table_bt",
      anim = F,
      condition =  !is.null(req(tbl_pairs_bt())))
    toggle(
      id = "box_plot_bt",
      anim = F,
      condition =  !is.null(req(tbl_pairs_bt())))
  })
  
  observe({
    req(tbl_pairs())
    toggle(
      id = "pcci_plot",
      anim = F,
      condition =  !is.null(req(tbl_pairs())))
    toggle(
      id = "box_plot",
      anim = F,
      condition =  !is.null(req(tbl_pairs())))
  })
  
  observeEvent(
    input$agree_contribute,
    toggle(id = "user_details",condition = input$agree_contribute)
  )
  
  ## load example  ------------------------------------------------------
  
  session$onFlushed(function() {
    isolate( 
      if (values$examples_completed < input$load_example_button)
      {
        updateSelectInput(session = session, inputId = "procedure_name",selected = "Body Composition (DEXA lean/fat)")
        updateTabsetPanel(session = session, inputId = "input_method",selected = "file")
        updateSelectizeInput(session = session, inputId = "measure_selected",selected = "Fat/Body weight")
        updateCheckboxInput(session,inputId = "agree_contribute", value = TRUE)
        values$file <- "example"
        values$example_step <- values$example_step + 1
        print(values$example_step)
      }
    )
  },once = F)
  
  ## end example cycle
  observeEvent(
    values$example_step,
    if (
      values$examples_completed < input$load_example_button &
      nrow(req(file_summaries())) == 4 &
      values$example_step > 40)
    {
      showModal(
        modalDialog(
          "Example input loaded.",tags$br(),"Click ",tags$b("Calculation Comparisons")," button to get the analysis results.",
          title = NULL,
          footer = modalButton("Ok"),
          size = "m",
          easyClose = T)
      ) 
      values$examples_completed <- input$load_example_button
      values$example_step <- 0
      enable("submit")
    })
    
              
  



  #   req(values$examples_completed < input$load_example_button) 
  #   
  # })
  
  #  
  # observeEvent(
  #   input$measure_selected,
  #   {
  #     
  #     #values$load_example_table <- T
  #     # session$sendCustomMessage(type = "updateFileInputHandler", 'upload_file')
  #   }
  # )
  # observeEvent(
  #   tbl_raw_data(),
  #   {
  #     req(values$examples_completed < input$load_example_button) 
  #     values$examples_completed <- input$load_example_button
  #   }
  # )
}
  
  ## drafts ------------------------------------------------------
  # prepare txt file to download
  # output$download_button <- downloadHandler(
  #   filename = function()
  #     paste0("Results_",Sys.Date(),".txt"),
  #   content  = function(file)
  #   {
  #     to_print <- list(
  #       groups = input$groups,
  #       procedure_name = input$procedure_name,
  #       send_data = input$send_data,    
  #       genotypes_tested = values$genotypes_tested,
  #       tbl_models_selected = values$tbl_models_selected,
  #       tbl_metadata_selected = values$
  #       ++_selected,
  #       tbl_raw_input = values$tbl_raw_input,
  #       tbl_summaries_from_file = values$tbl_summaries_from_file,
  #       tbl_summaries = values$tbl_summaries,
  #       tbl_pairs_calc = values$tbl_pairs_calc
  #       )
  #     sink(file = file)
  #     print(to_print)
  #     sink()
  #   }  
  # )

  # observe({
  #   req(input$submit)
  #   shinyjs::removeClass(id = "download_button", class = "disabled")
  # })

  # observeEvent(input$exmp_1,values$example_id_clicked <- 1)
  # 
  # observeEvent(
  #   values$example_id_clicked,
  #   {
  #     tbl_examples_selected <- tbl_examples %>% 
  #       filter(example_id == values$example_id_clicked)
  #     updateSelectInput(session = session, inputId = "procedure_name",selected = tbl_examples_selected$procedure_name)
  #     values$example_sequence_step <- TRUE
  #   }
  # )
  # 
  # observe(
  #   {
  #     req(values$example_sequence_on)
  #     tbl_examples_selected <- tbl_examples %>% 
  #       filter(example_id == values$example_id_clicked)
  #     updateSelectizeInput(session = session, inputId = "measure_selected",selected = tbl_examples_selected$parameter_name)
  #   }
  # )