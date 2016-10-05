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
  
  ## pressing on "read more" directs to instruction tab ----
  observeEvent(
    input$read_more,
    updateNavbarPage(session = session,inputId = "nav",selected = "Information")
    )
  
  #addClass(class = "table-gxl", selector = "#table-metadata, #table-details, #table-groups, #file_summaries")
  
  ## initialize reactive values: ----
  values <- reactiveValues(
    )

  ## create metadata input object for selected procedure: ----
  output$metadata_input <- renderUI({
    req(input$procedure_name)
    
    selected_url <- 
      tbl_procedures_1 %>% 
      filter(procedure_name == input$procedure_name) %>%
      select(url) %>% 
      .$url
    tbl_metadata_selected <- tbl_metadata_1 %>% 
      filter(procedure_name == input$procedure_name)
    measure_name_list <-
      tbl_measure_1 %>%
      filter(procedure_name == input$procedure_name) %>% 
      .$parameter_name %>%
      as.character() %>%
      as.list() %>%
      {setNames(.,.)}
    
    tags$table(
      id = "table-metadata",
      class = "table table-bordered table-gxl",
      tags$caption(
        "Parameters for ",
        input$procedure_name,
        " (",
        a(href = selected_url,target='_blank',"Standard Operating Procedure at IMPRESS"),
        ")"
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
      ),
      tags$tfoot(
        # tags$tr(
        #   tags$td(colspan = 3,
        #     tags$hr()
        #   )
        # ),
        tags$tr(
          tags$th("Meassure"),
          tags$td(
            selectizeInput(
              inputId = "measure_selected",
              label = NULL,
              choices = measure_name_list,
              options = list(create = TRUE))),
          tags$td("")
        )
      )
    )
  })
  
  ## create link to SOP page at IMPRESS for selected procedure: ----
  # output$proc_SOP_link <- renderUI({
  #   req(input$procedure_name)
  #   
  #   selected_url <- 
  #     tbl_procedures_1 %>% 
  #     filter(procedure_name == input$procedure_name) %>%
  #     select(url) %>% 
  #     .$url
  #   
  #   a(href = selected_url,
  #     target='_blank',
  #     "Link to Standard Operating Procedure")
  # })
  
  get_gxl_matchs <- reactive(
    {
      req(input$measure_selected,input$procedure_name,input$INT_001)
      tbl_measure_1 %>%
        filter(procedure_name == input$procedure_name,
               parameter_name==input$measure_selected,
               sex == input$INT_001)
    }
  )
  
  output$series_input <- renderUI(
    {
      req(get_gxl_matchs(),nrow(get_gxl_matchs())>1)
      numericInput(
        inputId = "duration",
        label = "Duration at measurement",
        min = 0,
        max = 40,
        value = 5
      )
    }
  )
  
  get_gxl_estimate <- reactive(
    {
      req(get_gxl_matchs(),input$duration)
      req(nrow(get_gxl_matchs())>1)
      x_pred <- input$duration
      x <- get_gxl_matchs()$duration
      y <- get_gxl_matchs()$s2_ratio  ## TODO: predict s2_ratio or s2_interaction

      y_pred <- predict.lm (lm(y~x,data = data.frame(y,x)), newdata = data.frame(x = x_pred))

      return(y_pred)
    }
  )
  
  collect_gxl_details <- reactive(
    {
      req(get_gxl_matchs())
      if(nrow(get_gxl_matchs())<2)
        get_gxl_matchs()
      else
      {
        req(get_gxl_estimate())
        s2_ratio_1 = get_gxl_estimate()
        duration_1 <- input$duration
        
        get_gxl_matchs() %>%
          mutate(duration = duration_1, s2_ratio =  s2_ratio_1) %>% 
          head(1) # TODO: should be distinct()
      }
        
    }
  )
      
      
  
  
  
  ## render details table for selected measure: ----
  output$selected_measure_details <- renderUI({
    req(collect_gxl_details())
    with(
      collect_gxl_details(),
      withMathJax(
        tags$table(
          id = "table-details",
          class = "table table-bordered table-gxl",
          tags$thead(
            tags$tr(
              tags$th(
                "\\(S^2_{int.}/S^2_{error}=\\)"
              ),
              tags$td(
                s2_ratio %>% signif(digits=5)
              )
            ),
            tags$tr(
              tags$th(
                "Units:"
              ),
              tags$td(
                unit
              )
            ),
            tags$tr(
              tags$th(
                "Transformation:"
              ),
              tags$td(
                transformation_symbol
              )
            )
          )
        )
      )
    )
  })
  
  ## show\hide input methods alternating ----
  # observeEvent(
  #   input$input_method,
  #   {
  #     toggle("file_form",condition = input$input_method=="file")
  #     toggle("groups",condition = input$input_method=="summ")
  #   }
  # )
  
  ## create input object to recieve raw data file: ----
  output$file_form <- renderUI({
    req(input$input_method=="file")
    #input$procedure_name
    tagList(
      br(),br(),
      fileInput(
      inputId = "upload_file",
      label = "Upload CSV file (no header):",
      accept = 'text/csv',
      width="80%"),
      #actionButton("reset_upload","Reset",style = "size:80%"), #class="ResetBtn",icon = icon("refresh"),
      downloadButton(outputId = "example_raw_input",label = "Example input file")
    )
  })
  
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
      req(input$upload_file,input$input_method=="file")
      read.csv(
          file = input$upload_file$datapath,
          header = F,
          stringsAsFactors = F
      )
    })  
  observe(print(input$upload_file))
  ## summarize and transform raw table: ----
  tbl_summaries <- reactive(
    {
      req(get_raw_df(),collect_gxl_details(),input$input_method=="file")
      trans_fun<- eval(parse(text=paste("function(x)",collect_gxl_details()$transformation_expr)))
      get_raw_df()  %>% 
        mutate(transformed = trans_fun(V2)) %>% 
        group_by(V1) %>% 
        summarise(mean.t = mean(transformed,na.rm = T),
                  mean = mean(V2,na.rm = T),
                  sd.t =sd(transformed,na.rm = T),
                  sd =sd(V2,na.rm = T),
                  n =sum(!is.na(transformed))) %>% 
        add_rownames() %>% 
        rename(group_id  = rowname, group_name = V1)
    })
    
 
  ## render summaries table as input for additional edits: ----
  
  output$file_summaries <- renderTable(
    {
      req(tbl_summaries())
      tbl_summaries() %>% select(Group = group_name, Mean = mean.t,`Standard Deviation`	= sd.t,`Num. of Observations` =	n)
    }
  )
  
  output$groups_summaries <- renderUI(
    {
      req(input$groups,input$input_method=="summ")
      tags$table(
        id = "table-groups",
        class = "table table-condensed table-gxl",
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
  
  ## link the example raw data file:  ----
  output$example_raw_input <- downloadHandler(
    filename = "Simplified IPGTT Glucose response AUC - example.csv",
    content = function(con) {
      write.table(x = tbl_example_measure_input,file = con,row.names = F,col.names = F, qmethod = "double",sep = ",")
    }
  )
  
  ## read input objects and create summaries table and pairs table: ----
  tbl_summaries_tidy <- eventReactive(eventExpr = 
    input$submit,valueExpr = 
    {
      req(input$submit,collect_gxl_details())
      
      s2_interaction <- collect_gxl_details()$s2_interaction
      n_labs_s2gxl <- collect_gxl_details()$n_labs_s2gxl
      n_groups_s2gxl <- collect_gxl_details()$n_groups_s2gxl
     
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
          df_gxl = (s2_pooled + 2*s2_interaction )^2  /( s2_pooled^2/(sum(n)-n()) + (2*s2_interaction)^2 /( (n_labs_s2gxl-1)*(n_groups_s2gxl-1) ) )
        )
    })
  
  observe(print(req(tbl_summaries_tidy())))
  
  ## calculate (estimate, se, statistic ,p-value, conf.lower, conf.higher)X(unadjusted, adjusted GxL) ----
  tbl_pairs <- reactive(
    {
      req(tbl_summaries_tidy(),collect_gxl_details())
      alpha <- input$alpha
      s2_interaction <- collect_gxl_details()$s2_interaction
      back_transform_expr <- collect_gxl_details()$back_transform_expr
      back_trans_fun <- eval(parse(text=paste("function(y)",back_transform_expr)))
      
      ## generate all pairs combinations:
      pairs_ind_comb <- nrow(tbl_summaries_tidy()) %>%
        combn(2) %>%
        t() %>%
        as.data.frame(stringsAsFactors=FALSE) %>%
        tbl_df() %>%
        transmute(group_id1 = as.factor(V1),group_id2 = as.factor(V2))

      ## create pairs table:
      tbl_pairs <- pairs_ind_comb %>%
        inner_join(tbl_summaries_tidy(),by = c("group_id1"="group_id")) %>%
        inner_join(tbl_summaries_tidy(),c("group_id2"="group_id","s2_pooled","df","df_gxl")) %>% 
        transmute(
          pair_id = paste0(group_id1,"-",group_id2),
          grp1  = group_name.x,
          grp2  = group_name.y,
          #`Pair Names` paste(group_name.x,"-",group_name.y), # term
          diff  = mean.x-mean.y,                       # estimate
          se    = sqrt(s2_pooled*(1/n.x+1/n.y)),
          se_gxl= sqrt(s2_pooled*(1/n.x+1/n.y)+2*s2_interaction),
          stat    = abs(diff)/se,
          stat_gxl= abs(diff)/se_gxl,
          pv      = 2*(1-pt(q = stat    ,df = df))     %>% pmin(1),
          pv_gxl  = 2*(1-pt(q = stat_gxl,df = df_gxl)) %>% pmin(1),
          Q_ = 1,
          Q_gxl = 1,
          df,
          df_gxl
        ) %>% 
        {
          if(input$fdr_adjust)
            mutate(
              .,
              pv = p.adjust(pv,"BH"),
              pv_gxl = p.adjust(pv_gxl,"BH"),
              Q_ = sum(pv <= alpha) / n(),
              Q_gxl = sum(pv_gxl <= alpha) / n()
            )
          else
            .
        } %>% 
        mutate(
          lwr = if (Q_==0) NA else diff - qt(1-alpha/2*Q_ ,df = df)*se %>% as.numeric(),
          upr = if (Q_==0) NA else diff + qt(1-alpha/2*Q_ ,df = df)*se %>% as.numeric(),
          lwr_gxl = if (Q_gxl==0) NA else diff - qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric(),
          upr_gxl = if (Q_gxl==0) NA else diff + qt(1-alpha/2*Q_gxl ,df = df_gxl)*se_gxl %>% as.numeric()
        )
      
      tbl_pairs_bt <- tbl_pairs %>% 
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
      return(list(tbl_pairs = tbl_pairs,tbl_pairs_bt = tbl_pairs_bt))
    })

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
        data = tbl_pairs()$tbl_pairs %>% select(grp1,grp2,diff,pv,lwr,upr,pv_gxl,lwr_gxl,upr_gxl),
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
          if(collect_gxl_details()$transformation_symbol == "none")
          {
            tags$caption(
              'Pairwise comparisons differences', 
              if (input$fdr_adjust) {' (BH adjusted for selection):'} else ':')
          } else 
          {
            tags$caption(
              'Table 1: Pairwise comparisons differences on ',tags$strong('transformed'),' scale',
              if (input$fdr_adjust) {' (BH adjusted for selection):'} else ':')
          }
        ) %>% formatSignif(3:9,digits = 5)
    }
  )
  
  ## render DT2 (back transformed): ----
  output$results_table_bt <- renderDataTable(
    expr = 
    {
      req(tbl_pairs(),collect_gxl_details()$transformation_symbol != "none")
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
        data = tbl_pairs()$tbl_pairs_bt %>% select(grp1,grp2,diff,pv,lwr,upr,pv_gxl,lwr_gxl,upr_gxl),
        rownames = FALSE,
        container =
          withTags(
            table(
              thead(
                tr(
                  th(rowspan = 2, 'Group 1'),
                  th(rowspan = 2, 'Group 2'),
                  th(rowspan = 2, 'Difference (G1 -G2)'),
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
            'Table 2: Pairwise comparisons differences on ',tags$strong('original'),' scale',
            if (input$fdr_adjust) {' (BH adjusted for selection):'} else ':')
        ) %>% formatSignif(3:9,digits = 5)
      }
  )
  
  ## prepare txt file to download: ----
  
  # output$download_button <- downloadHandler(
  #   filename = function()
  #     paste0("Results_",Sys.Date(),".txt"),
  #   content  = function(file)
  #   {
  #     to_print <- list(
  #       alpha = input$alpha,
  #       groups = input$groups,
  #       procedure_name = input$procedure_name,
  #       send_data = input$send_data,    
  #       genotypes_tested = values$genotypes_tested,
  #       tbl_measures_selected = values$tbl_measures_selected,
  #       tbl_metadata_selected = values$tbl_metadata_selected,
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
    {
      if(input$checkbox_agrees_share)
        shinyjs::enable(id = "email")
      else
        shinyjs::disable(id = "email")
    }
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

  
  