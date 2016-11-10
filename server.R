function(input, output, session) {
  
  ## initialize reactive values: ----
  values <- reactiveValues(
    raw_data_file = NULL
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
  
  ## if measure has duration (it is series) then add input for the duration:----
  observe({
    toggle(id = "duration_line",anim = F, condition = !is.null(tbl_matched_models()) & all( !is.na(tbl_models_selected$duration) ))
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
      if ( all( !is.na(tbl_matched_models$duration) ) )
      {
        x_pred <- input$duration
        x <- tbl_matched_models()$duration
        y <- tbl_matched_models()$s2_ratio  
        y_pred <- predict.lm(lm(y~x,data = data.frame(y,x)), newdata = data.frame(x = x_pred))
        
        tbl_matched_model <- tbl_matched_models %>% 
          mutate(duration = x_pred, s2_ratio =  y_pred) %>% 
          distinct()
      }
      else
        tbl_matched_model <- tbl_matched_models
      
      if(nrow(tbl_matched_model>1)) stop("Too many matchs.")
      
      tbl_matched_model
    }
  )

  output$unit <- renderUI({
    req(tbl_matched_model())
    tbl_matched_model()$unit
  })
  
  
  ## render details table for selected measure: ----
  output$selected_measure_details <- renderUI({
    req(tbl_matched_model())
    withTags(
      with(
        tbl_matched_model(),
        ul(
          li(
            b("GxL replicability ratio estimate: "),
            withMathJax("\\(S^2_{G\\times L}/S^2_{error}=\\)"),
            s2_ratio %>% signif(digits=6)
          ),
          li(
            if (tbl_matched_model()$transformation_symbol != "none")
              list(b("Transformation taken prior to analysis: "), transformation_symbol) #
            else
              b("No transformation is taken prior to analysis")
          )
        )
      )
    )
  })
  
  ## create input object to receive raw data file: ----
  output$file_form <- renderUI({
    req(input$input_method=="file")
    input$reset_upload
    #take dependencies on previous selections
    input$procedure_name
    input$measure_selected
    
    div(
      actionButton(inputId = "reset_upload",label = "Reset", icon = icon("refresh"),class = "btn_right btn-sm"),
      downloadButton(outputId = "example_raw_input",label = "Example input file", class = "btn_right btn-sm"),
       #class="ResetBtn",,
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
  })
  
  observe({
    
    #take dependencies on previous selections
    input$procedure_name
    input$measure_selected
    input$reset_upload
    values$file <- NULL
  })
  
  observe({
    values$file <- input$upload_file
  })
  
  ## link the example raw data file:  ----
  output$example_raw_input <- downloadHandler(
    filename = "Simplified IPGTT Glucose response AUC - example.csv",
    content = function(con) {
      write.table(x = tbl_example_measure_input,file = con,row.names = F,col.names = F, qmethod = "double",sep = ",")
    }
  )
  
  output$groups_form <- renderUI({
    req(input$input_method=="summ")
    selectizeInput(
      inputId = "groups",
      label = "Genotypes tested:",
      choices = genotype_list,
      multiple = T,
      options = list(create = TRUE))
  })
  
  # observeEvent(
  #   eventExpr = {
  #     # input$procedure_name
  #   },handlerExpr = {
  #     reset("upload_file")
  #   })
  
  ## read raw data from file: ----
  get_raw_df <- reactive(
    {
      req(values$file,input$input_method=="file")
      read.csv(
          file = values$file$datapath,
          header = F,
          stringsAsFactors = F
      )
    })  
  
  observe(print(values$file))
  
  ## summarize and transform raw table: ----
  tbl_summaries <- reactive(
    {
      req(get_raw_df(),tbl_matched_model(),input$input_method=="file")
      trans_fun <- eval(parse(text=paste("function(x)",tbl_matched_model()$transformation_expr)))
      get_raw_df()  %>% 
        mutate(group_name = as.factor(V1), transformed = trans_fun(V2)) %>% 
        group_by(group_name) %>% 
        summarise(mean.t = mean(transformed,na.rm = T),
                  mean = mean(V2,na.rm = T),
                  sd.t =sd(transformed,na.rm = T),
                  sd =sd(V2,na.rm = T),
                  n =sum(!is.na(transformed))) %>% 
        rownames_to_column(var = "group_id")
    })
    
 
  ## render summaries table as input for additional edits: ----
  
  output$file_summaries <- renderDataTable(
    {
      req(tbl_summaries())
      datatable(
        caption = "Groups summaries",
        data =  tbl_summaries() %>% select(Group = group_name, Mean = mean.t,`Standard Deviation`	= sd.t,`Num. of Observations` =	n)
      ) %>% formatSignif(2:3,digits = 5)
    }
  )
  
  output$groups_summaries <- renderUI(
    {
      req(input$groups,input$input_method=="summ")
      tags$table(
        id = "table-groups",
        class = "table table-gxl",
        tags$thead(
          tags$tr(
            tags$th("Group"),tags$th("Mean"),tags$th("Standard Deviation"),tags$th("Num. of Observations")
          )
        ),
        tags$tbody(
          lapply(
            1:length(input$groups),function(group_id)
            {
              tags$tr(
                tags$td(input$groups[group_id]),
                tags$td(numericInput(inputId = paste("grp",group_id,input$groups[group_id],"mean.t",sep = "_"), label = NULL, value = "")),
                tags$td(numericInput(inputId = paste("grp",group_id,input$groups[group_id],"sd.t",sep = "_"), label = NULL, value = "", min = 0)),
                tags$td(numericInput(inputId = paste("grp",group_id,input$groups[group_id],"n",sep = "_"), label = NULL, value = "", step = 1,min = 0))
              )
            }
          )
        )
      )
    }
  )
  
  output$groups_info <- renderUI(
    {
      req(tbl_summaries_tidy())
      tags$table(
        id = "table-groups-info",
        class = "table table-bordered table-gxl",
        tags$thead(
          tags$tr(
            tags$th("Group"),tags$th("Background strain"),tags$th("Gene symbol"),tags$th("Treatment")
          )
        ),
        tags$tbody(
          lapply(
            1:length(tbl_summaries_tidy()$group_id),function(group_id)
            {
              tags$tr(
                tags$td(tbl_summaries_tidy()$group_name[group_id]),
                tags$td(selectizeInput(inputId = paste("grp_info_",group_id,input$groups[group_id],"bg_strain",sep = "_"), label = NULL, choices = {c(" ",bg_strain_vec) %>% {setNames(.,.)}},options = list(create = TRUE))),
                tags$td(selectizeInput(inputId = paste("grp_info_",group_id,input$groups[group_id],"gene_symbol",sep = "_"), label = NULL, choices = {c(" ",gene_symbol_vec) %>% {setNames(.,.)}},options = list(create = TRUE))),
                tags$td(selectizeInput(inputId = paste("grp_info_",group_id,input$groups[group_id],"treatment",sep = "_"), label = NULL,   choices = {c(" ",treatment_vec) %>% {setNames(.,.)}},options = list(create = TRUE)))
              )
            }
          )
        )
      )
    }
  )
  
  
  
    # {
    #   tags$table(
    #     class = "table table-condensed",
    #     tags$thead(
    #       tags$tr(
    #         tags$th("Group"),tags$th("Mean"),tags$th("Standard Deviation"),tags$th("Num. of Observations")
    #       )
    #     ),   
    #     tags$tbody(
    #       
    #       values$tbl_summaries_from_file %>%
    #         mutate(group_id = 1:n()) %>% 
    #         rowwise() %>% 
    #         transmute(
    #           html_row =
    #             tags$tr(   
    #               tags$td(V1),
    #               tags$td(mean_t),#tags$td(numericInput(inputId = paste("grp",group_id,V1,"mean",sep = "_"), label = NULL, value = mean_t)),
    #               tags$td(sd_t),#tags$td(numericInput(inputId = paste("grp",group_id,V1,"sd",sep = "_"), label = NULL, value = sd_t, min = 0)),
    #               tags$td(n)#tags$td(numericInput(inputId = paste("grp",group_id,V1,"n",sep = "_"), label = NULL, value = n, step = 1,min = 0))
    #             ) %>%
    #             as.character()
    #         ) %>% 
    #         .$html_row %>% 
    #         HTML()
    #     )
    #   )
    # } else 
    #   if(!is.null(input$groups))
    #   {
  
  
  
  ## read input objects and create summaries table and pairs table: ----
  tbl_summaries_tidy <- reactive(
    {
      req(tbl_matched_model())
      
      s2_interaction <- tbl_matched_model()$s2_interaction
      n_labs_s2gxl <- tbl_matched_model()$n_labs_s2gxl
      n_groups_s2gxl <- tbl_matched_model()$n_groups_s2gxl
     
      if(input$input_method=="file")
      {
        tbl_summaries_tidy <- tbl_summaries()
      } else
      if(input$input_method=="summ")
      {
        ## get only summ table input ids 
        tbl_summaries_ids <- str_split_fixed(names(input),"_",n = 4) %>%
          as.data.frame() %>%
          tbl_df() %>%
          filter(V1 == "grp") %>% 
          mutate(input_name = paste(V1,V2,V3,V4,sep = "_"))
        
        ## read corresponding input values:
        tbl_summaries_values <- sapply(tbl_summaries_ids$input_name, function(x) input[[x]]) %>% 
          data_frame(input_name = names(.),value = .)
        
        tbl_summaries_tidy <- 
          inner_join(
            tbl_summaries_ids,
            tbl_summaries_values) %>% 
          select(group_id = V2, group_name = V3, key = V4, value) %>% 
          spread(key = key,value = value) %>% 
          filter(.,complete.cases(.))
      }
      
      ## calculate S2_pooled, df and gxl-adjusted df
      tbl_summaries_tidy %>%
        mutate(
          df = sum(n)-n(),
          s2_pooled = sum(sd.t^2*(n-1))/df,
          ## Satterthwaite approximation for pooled
          df_gxl = (s2_pooled + 2*s2_interaction )^2  /( s2_pooled^2/(sum(n)-n()) + (2*s2_interaction)^2 /( (n_labs_s2gxl-1)*(n_groups_s2gxl-1) ) ),
          x = 0,
          y = mean.t
        )
    })
  
  ## calculate (estimate, se, statistic ,p-value, conf.lower, conf.higher)X(unadjusted, adjusted GxL) ----
  tbl_pairs <- eventReactive(
    eventExpr = input$submit,
    valueExpr = 
    {
      req(input$submit,tbl_summaries_tidy())
    
      alpha <- 1-input$conf_level
      s2_ratio <- tbl_matched_model()$s2_ratio
      tbl_summaries <- tbl_summaries_tidy()
        

      ## generate all pairs combinations:
      pairs_ind_comb <- nrow(tbl_summaries) %>%
        combn(2) %>%
        t() %>%
        as.data.frame(stringsAsFactors=FALSE) %>%
        tbl_df() %>%
        transmute(group_id1 = as.character(V1),group_id2 = as.character(V2))
      
      nmeans_tukey <-  nrow(tbl_summaries)

      ## create pairs table:
      tbl_pairs <- pairs_ind_comb %>%
        inner_join(tbl_summaries,by = c("group_id1"="group_id")) %>%
        inner_join(tbl_summaries,c("group_id2"="group_id","s2_pooled","df","df_gxl")) %>% 
        transmute(
          pair_id = paste0(group_id1,"-",group_id2),
          name_pair = paste(group_name.x,"-",group_name.y),
          grp1  = group_name.x,
          grp2  = group_name.y,
          #`Pair Names` paste(group_name.x,"-",group_name.y), # term
          diff  = mean.x-mean.y,                       # estimate
          se    = sqrt(s2_pooled*(1/n.x+1/n.y)),
          se_gxl= sqrt(s2_pooled*(1/n.x+1/n.y+2*s2_ratio)),
          stat    = abs(diff)/se,
          stat_gxl= abs(diff)/se_gxl,
          pv      = 2*(1-pt(q = stat    ,df = df))     %>% pmin(1),
          pv_gxl  = 2*(1-pt(q = stat_gxl,df = df_gxl)) %>% pmin(1),
          Q_ = 1,
          Q_gxl = 1,
          df,
          df_gxl,
          x.x,
          y.x,
          x.y,
          y.y
        ) %>%
        {
          if (input$mult_adjust == "Tukey HSD")
            mutate(
              .,
              pv = ptukey(q = stat,nmeans = nmeans_tukey , df = df, lower.tail = FALSE) %>% pmin(1),
              pv_gxl = ptukey(q = stat_gxl, nmeans = nmeans_tukey, df = df_gxl ,lower.tail = FALSE) %>% pmin(1),
              lwr  = diff - qtukey(p = 1-alpha/2, nmeans = nmeans_tukey, df = df)*se/sqrt(2),
              upr  = diff + qtukey(p = 1-alpha/2, nmeans = nmeans_tukey, df = df)*se/sqrt(2),
              lwr_gxl = diff - qtukey(p = 1-alpha/2, nmeans = nmeans_tukey, df = df_gxl)*se_gxl/sqrt(2),
              upr_gxl = diff + qtukey(p = 1-alpha/2, nmeans = nmeans_tukey, df = df_gxl)*se_gxl/sqrt(2))
          else
            if(input$mult_adjust == "BH selected")
              mutate(
                .,
                pv = p.adjust(pv,"BH"),
                pv_gxl = p.adjust(pv_gxl,"BH"),
                Q_ = max(1,sum(pv <= alpha)) / n(),
                Q_gxl = max(1,sum(pv_gxl <= alpha)) / n(),
                lwr = diff - qt(1-alpha/2*Q_ ,df = df)*se %>% as.numeric(),
                upr = diff + qt(1-alpha/2*Q_ ,df = df)*se %>% as.numeric(),
                lwr_gxl = diff - qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric(),
                upr_gxl = diff + qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric())
            else 
              mutate(
                .,
                lwr = diff - qt(1-alpha/2*Q_ ,df = df)*se %>% as.numeric(),
                upr = diff + qt(1-alpha/2*Q_ ,df = df)*se %>% as.numeric(),
                lwr_gxl = diff - qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric(),
                upr_gxl = diff + qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric()
              )
          browser()
        }
      return(tbl_pairs)
    })
  
  tbl_pairs_bt <- reactive(
    {
      req(tbl_matched_model()$transformation_symbol != "none",tbl_pairs())
      back_transform_expr <- tbl_matched_model()$back_transform_expr
      back_trans_fun <- eval(parse(text=paste("function(y)",back_transform_expr)))
      
      tbl_pairs() %>% 
        req() %>% 
        transmute(
          pair_id = pair_id,
          grp1 = grp1,
          grp2 = grp2,
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
        data = tbl_pairs() %>% select(grp1,grp2,diff,pv,lwr,upr,pv_gxl,lwr_gxl,upr_gxl),
        rownames = FALSE,
        container =
          withTags(
            table(
              thead(
                tr(
                  th(rowspan = 2, 'Group 1'),
                  th(rowspan = 2, 'Group 2'),
                  th(rowspan = 2, 'Difference (G1-G2)'),
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
          if(tbl_matched_model()$transformation_symbol == "none")
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
        ) %>% formatSignif(3:9,digits = 5)
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
        data = tbl_pairs_bt() %>% select(grp1,grp2,diff,pv,lwr,upr,pv_gxl,lwr_gxl,upr_gxl),
        rownames = FALSE,
        container =
          withTags(
            table(
              thead(
                tr(
                  th(rowspan = 2, 'Group 1'),
                  th(rowspan = 2, 'Group 2'),
                  th(rowspan = 2, 'Difference (G1-G2)'),
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
        )) %>% formatSignif(3:9,digits = 5)
      }
  )
  
  ## render pcci plot (back transformed): ----
  output$pcci_plot <- renderPlot(
    {
      po <<- list(tbl_pairs = req(tbl_pairs()),tbl_summaries = req(tbl_summaries_tidy()))
      plot_pcci(pcci_obj = po,title = paste("Means Differences Confidence Intervals of",tbl_matched_model()$parameter_name),
                ylab = 
                  if (tbl_matched_model()$transformation_symbol != "none")
                    paste(tbl_matched_model()$parameter_name,"(",tbl_matched_model()$unit,") after",tbl_matched_model()$transformation_symbol,"transformation")
                 else
                  paste(tbl_matched_model()$parameter_name,"(",tbl_matched_model()$unit,")")
      )
    }
  )

  ## render boxplot : ----
  output$box_plot <- renderPlot(
    {
      req(tbl_pairs())
      trans_fun<- eval(parse(text=paste("function(x)",tbl_matched_model()$transformation_expr)))
      
      ggplot(data = get_raw_df() %>% req() %>% 
               mutate(group_name = as.factor(V1), measure = trans_fun(V2))
             ) + 
        aes(x = group_name, y = measure) + 
        geom_boxplot() + #width = bw
        ylab(
          if (tbl_matched_model()$transformation_symbol != "none")
            paste(tbl_matched_model()$parameter_name,"(",tbl_matched_model()$unit,") after",tbl_matched_model()$transformation_symbol,"transformation")
          else
            paste(tbl_matched_model()$parameter_name,"(",tbl_matched_model()$unit,")")
        ) + 
        xlab("")+
        ggtitle(paste("Groups boxplots of",tbl_matched_model()$parameter_name)) +
        theme_minimal()
    })
    
  ## render boxplot (back transformed): ----
  output$box_plot_bt <- renderPlot(
    {
      req(tbl_pairs_bt())
      ggplot(data = 
               get_raw_df() %>% req() %>% 
               mutate(group_name = as.factor(V1), measure = V2)
      ) + 
        aes(x = group_name, y = measure) + 
        geom_boxplot() + #width = bw
        ylab(
          tbl_matched_model()$unit
        ) + 
        ggtitle(paste("Groups boxplots of",tbl_matched_model()$parameter_name," before transformation")) +
        theme_minimal()
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
  
  
  ## prepare txt file to download: ----
  
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
  #       tbl_summaries_tidy = values$tbl_summaries_tidy,
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
  
  observeEvent(
    input$checkbox_agrees_share,
    toggle(id = "user_details",condition = input$checkbox_agrees_share)
  )
  
  # observeEvent(
  #   input$download_button,
  #   {
  #     req(input$submit)
  #     if(input$checkbox_agrees_share)
  #     {
  #       file_name <- paste0(input$email,as.integer(Sys.time()),".txt")
  #       
  #       capture.output(
  #         {
  #           print("####---------------------------")
  #           print(Sys.time())
  #           print(input$email)
  #           print("####---------------------------")
  #           SDRAERYAESBYEASRBYE
  #         },
  #         file = file_name,
  #         append = T)
  #       
  #       drop_upload(
  #         file = file_name,
  #         dest = log_file_drop_dir,
  #         overwrite = F)
  #       unlink(file_name)
  #     }
  #   }
  # )
  
  
  
  
  ## prepare plots: ----
  
  
  # when main object created, plot cis
  # output$ci_plot <- renderPlot(
  #   {
  #     req(values$tbl_pairs_calc)
  #     
  #     values$tbl_pairs_calc %>% 
  #       select(-(1:13),-pair_id,-m,-R,-`R GxL-Adj`,-contains("Statistic"),-contains("df")) %>% 
  #       {
  #         if(input$back_transformed)
  #           select(.,contains("Group"),contains("(Orig. Scale)"))
  #         else
  #           select(.,-contains("(Orig. Scale)"),-contains("p-value"))
  #       } %>% 
  #       {
  #         if(input$fdr_adjust)
  #           select(.,contains("Group"),contains("Difference"),contains("BH-Adj."))
  #         else
  #           select(.,-contains("BH-Adj."),-contains("Standard"))
  #       } %>% 
  #       gather(key = key,value = value,-`Group 1`,-`Group 2`)
  #   }
  # )
}

  
  