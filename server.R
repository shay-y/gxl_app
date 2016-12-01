
  
function(input, output, session) {
  ##  create temporary debugging concole: ----
  # * observe(label="console",{
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
    n_loads_completed = 0,
    file = NULL
    )

  ## * metadata input: create metadata and measure inputs table for the selected procedure: ----
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
  
  ## * metadata input: render unit cell ----
  output$unit <- renderUI({
    req(input$measure_selected,input$procedure_name)
    tbl_models %>%
      filter(procedure_name == input$procedure_name,
             parameter_name == input$measure_selected) %>% 
      distinct(unit) %>% .$unit
  })
  
  ##  * metadata input: if measure has duration (it is series) then add input for the duration:----
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
      
      ## no model found
      # if(nrow(tbl_matched_models)==0) {
      #   tbl_matched_model_ %>% add_row() ...
      #   }
      
      ## if duration related meassure, estimate gxl ratio:
      if ( nrow(tbl_matched_models)>0 & all( !is.na(tbl_matched_models$duration) ) )
      {
        x_pred <- input$duration
        x <- tbl_matched_models$duration %>% as.numeric()
        y <- tbl_matched_models$s2_ratio  
        y_pred <- approxfun(x,y,rule = 2)(x_pred)
        
        tbl_matched_model_ <- tbl_matched_models %>% 
          mutate(duration = x_pred, s2_ratio =  y_pred) %>% 
          select(-s2_interaction, -s2_lab, -s2_error) %>% 
          distinct()
      }
      else
        tbl_matched_model_ <- tbl_matched_models
      
      # debug
      if(nrow(tbl_matched_model_)>1) {cat("Too many matchs.")}

      return(tbl_matched_model_)
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
            if (tbl_matched_model()$transformation_expr != "x")
              list(b("Transformation taken prior to analysis: "), tbl_matched_model()$transformation_symbol) #
            else
              b("No transformation is applied for this measure")
          )
        )
      } else
      {
        ul(
          li(
            b("GxL replicability ratio estimate not found")
          ),
          li(
            b("No transformation is applied")
          )
        )
      }
    )
  })
  
  ## * file input: create input object to receive raw data file: ----
  output$file_form <- renderUI({
    req(input$input_method=="file")
    
    # take dependencies on previous selections and reset button
    input$procedure_name
    input$measure_selected
    input$input_method
    
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
  })
  
  ## * file input: pass file input to value to workaround resetting file data----
  observe({
    values$file <- input$upload_file
  })
 
  ## * file input: link the example raw data file:  ----
  output$example_raw_input <- downloadHandler(
    filename = "gxl_app_example_input.csv",
    content = function(con) {
      write.table(x = tbl_example_raw_data,file = con,row.names = F,col.names = F, qmethod = "double",sep = ",")
    }
  )
  
  ## * file input: read raw data from file: ----
  tbl_raw_data <- eventReactive(eventExpr = values$file,
    {
      req(input$input_method=="file")
      read.csv(
        file = values$file$datapath,
        header = F,
        stringsAsFactors = F
      )
    })  
  
  ## * file input: summarize and transform raw table: ----
  file_summaries <- reactive(
    {
      req(tbl_raw_data(),tbl_matched_model())
      # enable("submit")
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
    
  ## * file input: render file summaries table: ----
  
  output$file_summaries_table <- renderDataTable(
    {
      req(file_summaries())
      datatable(
        rownames = F,
        class = "compact",
        options = 
          list(
            dom = 'tB',
            buttons = c('csv','excel')),
        extensions = 'Buttons',
        caption = "Groups Summaries",
        data =  file_summaries() %>% select(Group = group_name, Mean = mean.t,`Standard Deviation`	= sd.t,`Num. of Observations` =	n)
      ) %>% formatSignif(2:3,digits = 3)
    }
  )
  
  ## * summaries input: create input form for groups summaries input fields: ----
  output$groups_form <- renderUI({
    input$procedure_name
    input$measure_selected
    input$input_method
    req(input$input_method=="summ")
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
          1:input$n_group_inputs,function(i)
          {
            tags$tr(
              tags$td(selectizeInput( inputId = paste0("grp",i,"_name"  ), label = NULL, choices = c("",group_names_list),selected = "",multiple = F, options = list(create = TRUE))),
              tags$td(  numericInput( inputId = paste0("grp",i,"_mean.t"), label = NULL, value = "")),
              tags$td(  numericInput( inputId = paste0("grp",i,"_sd.t"  ), label = NULL, value = "", min = 0)),
              tags$td(  numericInput( inputId = paste0("grp",i,"_n"     ), label = NULL, value = "", step = 1,min = 0))
            )
          }
        )
      )
    )
  }
  )
  
  ## * summaries input: read from group summaries input fields to a tibble:----
  
  grps_summaries <- reactive(
    {
      ## initialize tibble
      tbl_grp_summ <- tibble(
        group_name = NA,
        mean.t = NA %>% as.numeric(),
        sd.t = NA %>% as.numeric(),
        n = NA %>% as.numeric())

              ## take dependencies and fill in current values 
      for(i in 1:input$n_group_inputs)
      {
        tbl_grp_summ[i,"group_name"] <- input[[paste0("grp",i,"_name"  )]] %>% ifelse(isTruthy(.),.,NA)
        tbl_grp_summ[i,"mean.t"] <- input[[paste0("grp",i,"_mean.t"  )]]   %>% ifelse(isTruthy(.),.,NA)
        tbl_grp_summ[i,"sd.t"] <- input[[paste0("grp",i,"_sd.t"  )]]       %>% ifelse(isTruthy(.),.,NA)
        tbl_grp_summ[i,"n"] <- input[[paste0("grp",i,"_n"  )]]             %>% ifelse(isTruthy(.),.,NA)
      }

      ## keep only complete cases (trims down incomplete input field lines)
      tbl_grp_summ_complete <- tbl_grp_summ %>%
        filter(group_name!="",isTruthy(mean.t),isTruthy(sd.t),isTruthy( n))
      
      print(tbl_grp_summ_complete)
      
      if (nrow(tbl_grp_summ_complete) > 1)
      {
        return(tbl_grp_summ_complete)
      } else NULL
    })
  
  ## render group info table: ----
  output$groups_info <- renderUI(
    {
      if (input$input_method=="file")
        groups <- req(file_summaries()$group_name)
      if (input$input_method=="summ")
        groups <- req(grps_summaries()$group_name)
      
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
  
  ## * on submit, copy summaries from the selected input method and add calculations: ----
  tbl_summaries <- reactive(
    {
      input$submit
      values$n_loads_completed
      
      tbl_summ <- switch(
        input$input_method,
        "file" = req(file_summaries()),
        "summ" = req(grps_summaries())
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
  
  ## * on submit, push data to server: ----
  
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
          grps_summaries())
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
  
  
  ## * calculate pairs with estimate, se, statistic ,p-value, conf.lower, conf.higher [unadjusted, adjusted GxL] ----
  tbl_pairs <- eventReactive({tbl_summaries(); input$conf_level; input$mult_adjust},
    {
      req(tbl_summaries(),tbl_matched_model())
      
      if (nrow(tbl_matched_model())!=0)
      {
        s2_ratio <- tbl_matched_model()$s2_ratio
        n_labs_s2gxl <- tbl_matched_model()$n_labs_s2gxl
        n_groups_s2gxl <- tbl_matched_model()$n_groups_s2gxl
      } else
      {
        s2_ratio      <- NA
        n_labs_s2gxl  <- NA
        n_groups_s2gxl<- NA
      }
      
      alpha <- 1-input$conf_level
      tbl_summ <- tbl_summaries()
      nmeans_tukey <-  nrow(tbl_summ)  
      
      ## generate all pairs combinations,
      ## create pairs table,
      ## calculate stats:
      
      if (nrow(tbl_summ) == 2)
        tbl_pairs_ <- tibble(V1 = 2, V2 = 1)
      else
        tbl_pairs_ <- nrow(tbl_summ) %>%  combn(2) %>% t() %>% .[,2:1] %>% as_data_frame()  
      
      tbl_pairs_ <- tbl_pairs_ %>% 
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
      
      if (tbl_matched_model()$transformation_expr != "x")
      {
        back_trans_fun <- eval(parse(text=paste("function(y)", tbl_matched_model()$back_transform_expr )))
        tbl_pairs_ <- tbl_pairs_ %>% 
          mutate(
            diff_bt  = back_trans_fun(diff),
            lwr_bt = back_trans_fun(lwr),
            upr_bt = back_trans_fun(upr),
            lwr_gxl_bt = back_trans_fun(lwr_gxl),
            upr_gxl_bt = back_trans_fun(upr_gxl)
          )
      }
      
      if (nrow(tbl_matched_model())==0)
        tbl_pairs_ <- tbl_pairs_ %>% select(-contains("gxl"))
      
      tbl_pairs_temp <<- tbl_pairs_
      return(tbl_pairs_)
    })

  ## render pairs table: ----
  output$pairs_table <- DT::renderDataTable(
    expr = 
    {
      req(tbl_pairs())
      if (nrow(tbl_matched_model())==0)
        tbl_pairs_to_print <- tbl_pairs() %>% select(name_pair,diff,pv,lwr,upr)
      else
      {
        if (!input$back_transform)
          tbl_pairs_to_print <- tbl_pairs() %>% select(name_pair,diff,pv,lwr,upr,pv_gxl,lwr_gxl,upr_gxl)
        else
          tbl_pairs_to_print <- tbl_pairs() %>% select(name_pair,diff_bt,pv,lwr_bt,upr_bt,pv_gxl,lwr_gxl_bt,upr_gxl_bt)
      }
      
      datatable(
        class = "compact", # BS: table table-striped table-bordered table-condensed table-hover
        options = 
          list(
            autoWidth = F,
            dom = 'tB',
            paging = F,
            scrollY = "300px",
            scrollCollapse = T,
            scrollX = TRUE, 
            buttons = c('csv','excel')),
        extensions = 'Buttons',
        data = tbl_pairs_to_print,
        rownames = FALSE,
        container =
          withTags(
            table(
              thead(
                tr(
                  th(rowspan = 2, 'Comparison'),
                  th(rowspan = 2, 'Difference'),
                  th(colspan = 3, 'Unadjusted'),
                  if (nrow(tbl_matched_model())!=0) th(colspan = 3, 'GxL-Adjusted')
                ),
                tr(
                  th('p-value'),
                  th('CI-Low'),
                  th('CI-High'),
                  if (nrow(tbl_matched_model())!=0) th('p-value'),
                  if (nrow(tbl_matched_model())!=0) th('CI-Low'),
                  if (nrow(tbl_matched_model())!=0) th('CI-High')
                )
              )
            )
          ),
        caption = "Groups Pairwise Comparisons"
          # if(isolate(expr = tbl_matched_model()$transformation_symbol) == "none")
          # {
          #   tags$caption(
          #     'ifferences', 
          #     if (input$mult_adjust == "BH selected") {' (BH adjusted for FDR control):'} else if (input$mult_adjust == "Tukey HSD") {' (Tukey HSD adjusted to control FWER):'} else ':')
          # } else 
          # {
          #   tags$caption(
          #     'Table 1: Pairwise comparisons differences on ',tags$strong('transformed'),' scale'
          #    # if (input$mult_adjust == "BH selected") {' (BH adjusted for FDR control):'} else if (input$mult_adjust == "Tukey HSD") {' (Tukey HSD adjusted to control FWER):'} else':')
          # )}
        ) %>% formatSignif(2:8,digits = 3)
    }
  )
  
  ## render pcci plot: ----
  output$pcci_plot <- renderPlot(
    {
      req(tbl_pairs())
      plot_pcci(
        tbl_pairs = tbl_pairs(),
        title = paste("Confidence Intervals of Means Differences ;",tbl_matched_model()$parameter_name),
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
      req(tbl_pairs())
      
      if (input$input_method!="file")
        "Boxplots avaliable on file input only"
      else
      {
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
      }
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
    toggle(
      id = "file_summaries",
      anim = F,
      condition =  isTruthy(file_summaries()))
  })
  
  observe({
    toggle(
      id = "pairs_table",
      anim = F,
      condition =  isTruthy(tbl_pairs()))
    toggle(
      id = "pcci_plot",
      anim = F,
      condition =  isTruthy(tbl_pairs()))
    toggle(
      id = "box_plot",
      anim = F,
      condition =  isTruthy(tbl_pairs()))
  })
  
  # observe({
  #   toggle(
  #     id = "",
  #     anim = F,
  #     condition =  isTruthy())
  # })
  
  observeEvent(
    input$agree_contribute,
    toggle(id = "user_details",condition = input$agree_contribute)
  )
  
  ## load example  ------------------------------------------------------
  
  
  observeEvent(priority = 2,
    input$load_example_button,
    {
      updateSelectInput(session = session, inputId = "procedure_name",selected = "")
      updateSelectInput(session = session, inputId = "procedure_name",selected = "Body Composition (DEXA lean/fat)")
      updateTabsetPanel(session = session, inputId = "input_method",selected = "summ")
    })
  
  observeEvent(
    priority = 1,
    {input$procedure_name;input$measure_selected},
    if(values$n_loads_completed < input$load_example_button)
    {
      updateSelectizeInput(session = session, inputId = "measure_selected",selected = "Fat/Body weight")
    })
  
  observeEvent(
    priority = 0,
    {input$measure_selected},
    updateNumericInput(session = session, inputId = "n_group_inputs",value = 4)
  )
    
  observeEvent(
    priority = -1,
    {input$n_group_inputs},
    if(values$n_loads_completed < input$load_example_button)
    {
      updateSelectizeInput(session = session, inputId = "grp1_name",selected = "C57BL/6")
      updateSelectizeInput(session = session, inputId = "grp2_name",selected = "Slc38a10")
      updateSelectizeInput(session = session, inputId = "grp3_name",selected = "Tnfaip1")
      updateSelectizeInput(session = session, inputId = "grp4_name",selected = "Ttll4")
      updateNumericInput(session = session, inputId = "grp1_mean.t",value = 0.48236	)
      updateNumericInput(session = session, inputId = "grp2_mean.t",value = 0.49274	)
      updateNumericInput(session = session, inputId = "grp3_mean.t",value = 0.42583	)
      updateNumericInput(session = session, inputId = "grp4_mean.t",value = 0.38516	)
      updateNumericInput(session = session, inputId = "grp1_sd.t",value = 0.057855)
      updateNumericInput(session = session, inputId = "grp2_sd.t",value = 0.054624)
      updateNumericInput(session = session, inputId = "grp3_sd.t",value = 0.046420)
      updateNumericInput(session = session, inputId = "grp4_sd.t",value = 0.079445)
      updateNumericInput(session = session, inputId = "grp1_n",value = 300)
      updateNumericInput(session = session, inputId = "grp2_n",value = 9)
      updateNumericInput(session = session, inputId = "grp3_n",value = 13)
      updateNumericInput(session = session, inputId = "grp4_n",value = 11)
    })
  
  observeEvent(
    priority = -2,
    {input$grp4_n},
    {
      req(input$grp4_n ==  11)
      if(values$n_loads_completed < input$load_example_button)
      {
        values$n_loads_completed <- input$load_example_button
        showNotification(
          id = "example_loaded_notification",
          duration = 6,closeButton = T,type = "warning",session = session,
          ui = 
            tagList(
              "Example loaded."# ,tags$br(),"Click ",tags$b("Calculation Comparisons"),"."
            )
        )
      }
    }
  )
  
  ## * resets : step 1 inputs   : ----
  
  observeEvent(
    priority = -3,
    input$reset_all,
    {
      values$n_loads_completed <- input$load_example_button
      reset(id = "table-groups-info")
      reset(id = "table-groups")
      reset(id = "n_group_inputs")
      updateTabsetPanel(session = session, inputId = "input_method",selected = "file")
      values$file <- NULL
      reset("measure_selected")
      reset("procedure_name")
    })
  
  ## * resets : step 2 inputs   : ----
  
  observeEvent(
    priority = 3,
    {
      input$procedure_name
      input$measure_selected
    },
    {
      if (values$n_loads_completed >= input$load_example_button)
      {
      reset(id = "table-groups-info")
      reset(id = "table-groups")
      reset(id = "n_group_inputs")
      updateTabsetPanel(session = session, inputId = "input_method",selected = "file")
      values$file <- NULL  
      }
    })
  
  observeEvent(
    {
      input$input_method
    },
    {
      if (values$n_loads_completed >= input$load_example_button)
      {
      reset(id = "table-groups-info")
      reset(id = "table-groups")
      reset(id = "n_group_inputs")
      values$file <- NULL  
      }
    })
  
  # observers ---------------------------------------------------------------
  
  
  # observe(print(req(input$input_method)))
  observe(print(values$file))
  # observe(print(req(file_summaries())))
  # observe(print(req(grps_summaries())))
  observe(print(req(tbl_summaries())))
  observe(print(req(tbl_pairs())))
}
  





