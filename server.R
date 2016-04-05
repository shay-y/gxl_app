
function(input, output, session) {
  
  ## create temporary debugging concole:
  observe(label="console",{
    if(input$console != 0) {
      options(browserNLdisabled=TRUE)
      saved_console<-".RDuetConsole"
      if (file.exists(saved_console)) load(saved_console)
      isolate(browser())
      save(file=saved_console,list=ls(environment()))
    }
  })
  
  ## initialize reactive values:
  values <- reactiveValues(
    genotypes_tested = NULL,
    tbl_metadata_selected = NULL,
    tbl_measures_selected = NULL,
    measure_name_list = NULL,
    tbl_summaries = NULL,
    tbl_raw_input = NULL)

  ## create metadata input object for selected procedure:
  output$metadata_input <- renderUI({
    req(input$procedure_name)
    
    values$tbl_metadata_selected <- tbl_metadata_1 %>% 
      filter(procedure_name == input$procedure_name)
    values$tbl_measures_selected <-
      tbl_measure_1 %>%
      filter(procedure_name == input$procedure_name)
    
    values$measure_name_list <-
      values$tbl_measures_selected %>% 
      .$parameter_name %>%
      as.character() %>%
      as.list() %>%
      {setNames(.,.)}
    
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
        ),
        tags$tr(
          br()
        ),
        tags$tr(
          tags$td("Meassure"),
          tags$td(
            selectizeInput(
              inputId = "measure_selected",
              label = NULL,
              choices = values$measure_name_list,
              options = list(create = TRUE))),
          tags$td("")
        )
      )
    )
  })
  
  ## populate measure input with measures avaliable for selected procedure:
  # observeEvent(values$measure_name_list,{
  #   updateSelectInput(session,"measure_name", choices = values$measure_name_list)
  # })
  
  ## create link to SOP page at IMPRESS for selected procedure:
  output$proc_SOP_link <- renderUI({
    req(input$procedure_name)
    
    selected_url <- 
      tbl_procedures_1 %>% 
      filter(procedure_name == input$procedure_name) %>%
      select(url) %>% 
      .$url
    
    a(href = selected_url,
      target='_blank',
      "Link to Standard Operating Procedure")
  })
  
  ## render details table for selected measure:
  output$selected_measure_details <- renderUI({
    req(input$measure_selected,input$procedure_name)
    values$measure_selected_row <-
      tbl_measure_1 %>%
      filter(procedure_name == input$procedure_name &
               parameter_name==input$measure_selected)
    with(
      values$measure_selected_row,
      withMathJax(
        "\\(S^2_{int.}/S^2_{error}=\\)",
        s2_ratio %>% format(digits=5,nsmall=3),
        br(),
        strong("Units:"),
        unit,
        br(),
        strong("Transformation:"),
        transformation_symbol
      )
    )
  })
  
  ## create input object to recieve raw data file:
  output$file_input <- renderUI({
    input$reset_upload
    fileInput(
      inputId = "upload_file",
      label = "Upload CSV file (no header):",
      accept = 'text/csv',
      width="80%")
  })
  
  ## reset objects raw table, summaries table and groups:
  observeEvent(input$reset_upload,{
    values$tbl_raw_input <- NULL
    values$tbl_summaries <- NULL
    reset("groups")
  })
  
  ## populate raw table with file input:
  observeEvent(input$upload_file,{
    values$tbl_raw_input <- 
      read.csv(
        file = input$upload_file$datapath,
        header = F,
        stringsAsFactors = F
      )
  },priority = -1)  
  
  ## summarize and transform raw table:
  observeEvent({values$tbl_raw_input;values$measure_selected_row},{
    req(nrow(values$measure_selected_row)!=0)
    trans_fun <- eval(parse(text=paste("function(x)",values$measure_selected_row$transformation_expr)))

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
 
  ## render summaries table as input for additional edits:
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
            mutate(group_id = 1:n()) %>% 
            rowwise() %>% 
            transmute(
              html_row =
                tags$tr(   
                  tags$td(V1),
                  tags$td(numericInput(inputId = paste("grp",group_id,V1,"mean",sep = "_"), label = NULL, value = mean_t)),
                  tags$td(numericInput(inputId = paste("grp",group_id,V1,"sd",sep = "_"), label = NULL, value = sd_t, min = 0)),
                  tags$td(numericInput(inputId = paste("grp",group_id,V1,"n",sep = "_"), label = NULL, value = n, step = 1,min = 0))
                ) %>%
                as.character()
            ) %>% 
            .$html_row %>% 
            HTML()
        } else NULL,
        
        if(!is.null(input$groups))
        {
          n_rows <- ifelse(!is.null(values$tbl_summaries),nrow(values$tbl_summaries),0) 
          lapply(
            n_rows + 1:length(input$groups),function(group_id)
            {
              tags$tr(
                tags$td(input$groups[group_id - n_rows]),
                tags$td(numericInput(inputId = paste("grp",group_id,input$groups[group_id - n_rows],"mean",sep = "_"), label = NULL, value = "")),
                tags$td(numericInput(inputId = paste("grp",group_id,input$groups[group_id - n_rows],"sd",sep = "_"), label = NULL, value = "", min = 0)),
                tags$td(numericInput(inputId = paste("grp",group_id,input$groups[group_id - n_rows],"n",sep = "_"), label = NULL, value = "", step = 1,min = 0))
              )
            }
          )
        } else NULL
      )
    )
  })
  
  ## link the example raw data file 
  output$example_raw_input <- downloadHandler(
    filename = "Simplified IPGTT Glucose response AUC - example.csv",
    content = function(con) {
      write.table(x = tbl_example_measure_input,file = con,row.names = F,col.names = F, qmethod = "double",sep = ",")
    }
  )
  
  # read input objects and create summaries table and pairs table
  observeEvent(
    {input$submit; values$measure_selected_row},
    {
      req(values$measure_selected_row,input$submit)
      attach(values$measure_selected_row) # for s2_interaction, n_labs_s2gxl, n_groups_s2gxl, back_transform_expr
      
      ## read input ids corresponding to the summaries table (including manual additions) and place in a df
      tbl_summaries_ids <- str_split_fixed(names(input),"_",n = 4) %>%
        as.data.frame() %>%
        tbl_df() %>%
        filter(V1 == "grp") %>% 
        mutate(input_name = paste(V1,V2,V3,V4,sep = "_"))
      
      ## read corresponding input values:
      tbl_summaries_values <- sapply(tbl_summaries_ids$input_name, function(x) input[[x]]) %>% 
        data_frame(input_name = names(.),value = .)
      #browser()
      ## combine both, calculate S2_pooled, df and gxl-adjusted df
      tbl_summaries_tidy <- 
        inner_join(
          tbl_summaries_ids,
          tbl_summaries_values) %>% 
        select(group_id = V2, group_name = V3, key = V4, value) %>% 
        spread(key = key,value = value) %>% 
        mutate(
          df = sum(n)-n(),
          s2_pooled = sum(sd^2*(n-1))/df,
          ## Satterthwaite approximation for pooled
          "df GxL-Adj." = (s2_pooled + 2*s2_interaction )^2  /( s2_pooled^2/(sum(n)-n()) + (2*s2_interaction)^2 /( (n_labs_s2gxl-1)*(n_groups_s2gxl-1) ) )
        )
      
      ## generate all pairs combinations:
      pairs_ind_comb <- nrow(tbl_summaries_tidy) %>% 
        combn(2) %>% 
        t() %>% 
        as.data.frame(stringsAsFactors=FALSE) %>% 
        tbl_df() %>% 
        transmute(group_id1 = as.factor(V1),group_id2 = as.factor(V2))

      ## create pairs table:
      values$tbl_pairs <- pairs_ind_comb %>% 
        inner_join(tbl_summaries_tidy,by = c("group_id1"="group_id")) %>% 
        inner_join(tbl_summaries_tidy,c("group_id2"="group_id","s2_pooled","df","df GxL-Adj."))
      
      # make two tables reactive:
      values$tbl_summaries_tidy  <- tbl_summaries_tidy  
      detach(values$measure_selected_row)
    })
  
  ## calculate (estimate, se, statistic ,p-value, conf.lower, conf.higher)X(unadjusted, adjusted GxL)X(unadjusted, adjusted BH)X(back transform yes\no)
  observeEvent(
    {values$tbl_pairs; input$alpha},
    {
      req(values$tbl_pairs,input$alpha)
      alpha <- input$alpha
      attach(values$measure_selected_row) # for s2_interaction, n_labs_s2gxl, n_groups_s2gxl, back_transform_expr
      back_trans_fun <- eval(parse(text=paste("function(y)",back_transform_expr)))

      values$tbl_pairs_calc <- values$tbl_pairs %>% 
        mutate(
          pair_id   = paste0(group_id1,"-",group_id2),
          `Pair Names` = paste(group_name.x,"-",group_name.y), # term
          Difference   = mean.x-mean.y,                       # estimate
          `Standard Error`      = sqrt(s2_pooled*(1/n.x+1/n.y)),
          `Standard Error GxL-Adj.` = sqrt(s2_pooled*(1/n.x+1/n.y)+2*s2_interaction),
          Statistic = Difference/`Standard Error`,
          `Statistic GxL-Adj.` = Difference/`Standard Error GxL-Adj.`,
          `p-value` = 2*pt(q = Statistic,df = df),
          `p-value GxL-Adj.` = 2*pt(q = `Statistic GxL-Adj.`,df = `df GxL-Adj.`),
          `p-value BH-Adj.` = p.adjust(`p-value`,"BH"),
          `p-value GxL-Adj. BH-Adj.` = p.adjust(`p-value GxL-Adj.`,"BH"),
          `CI-Low` = Difference - qt(1-alpha,df = df)*`Standard Error`,
          `CI-High` = Difference + qt(1-alpha,df = df)*`Standard Error`,
          `CI-Low  GxL-Adj.` = Difference - qt(1-alpha,df = df)*`Standard Error GxL-Adj.`,
          `CI-High GxL-Adj.` = Difference + qt(1-alpha,df = df)*`Standard Error GxL-Adj.`,
          m = n(),
          R = `p-value GxL-Adj.` <= alpha,
          `R GxL-Adj` = `p-value GxL-Adj. BH-Adj.` <= alpha,
          `CI-Low  BH-Adj.` = Difference - qt(1-alpha*R/m,df = df)*`Standard Error`,
          `CI-High BH-Adj.` = Difference + qt(1-alpha*R/m,df = df)*`Standard Error`,
          `CI-Low  GxL-Adj. BH-Adj.` = Difference - qt(1-alpha*`R GxL-Adj`/m,df = `df GxL-Adj.` )*`Standard Error GxL-Adj.`,
          `CI-High GxL-Adj. BH-Adj.` = Difference + qt(1-alpha*`R GxL-Adj`/m,df = `df GxL-Adj.` )*`Standard Error GxL-Adj.`,
          `Difference (Orig. Scale)` = back_trans_fun(Difference),
          `CI-Low  (Orig. Scale)` = back_trans_fun(`CI-Low`),
          `CI-High (Orig. Scale)` = back_trans_fun(`CI-High`),
          `CI-Low  GxL-Adj.(Orig. Scale)` = back_trans_fun(`CI-Low  GxL-Adj.`),
          `CI-High GxL-Adj.(Orig. Scale)` = back_trans_fun(`CI-High GxL-Adj.`),
          `CI-Low   BH-Adj.(Orig. Scale)` = back_trans_fun(`CI-Low  BH-Adj.`),
          `CI-High  BH-Adj.(Orig. Scale)` = back_trans_fun(`CI-High BH-Adj.`),
          `CI-Low  GxL-Adj. BH-Adj.(Orig. Scale)` = back_trans_fun(`CI-Low  GxL-Adj. BH-Adj.`),
          `CI-High GxL-Adj. BH-Adj.(Orig. Scale)` = back_trans_fun(`CI-High GxL-Adj. BH-Adj.`)
        )
      detach(values$measure_selected_row)
    })
  
  ## render DT 
  output$results_table <- renderDataTable(
    # {
    #   values$tbl_pairs_calc;
    #   input$fdr_adjust;
    #   input$gxl_adjust;
    #   input$back_transformed},
    {
      req(values$tbl_pairs_calc)
      
      values$tbl_pairs_calc %>% 
        select(-pair_id,-m,-R,-`R GxL-Adj`,-contains("Statistic")) %>% 
        {
          if(input$back_transformed)
            select(.,`Pair Names`,contains("(Orig. Scale)"),contains("p-value"))
          else
            select(.,-contains("(Orig. Scale)"))
        } %>% 
        {
          if(input$gxl_adjust)
            select(.,`Pair Names`,contains("Difference"),contains("GxL-Adj."))
          else
            select(.,-contains("GxL-Adj."))
        } %>% 
        {
          if(input$fdr_adjust)
            select(.,`Pair Names`,contains("Difference"),contains("Standard"),contains("BH-Adj."))
          else
            select(.,-contains("BH-Adj."))
        }
    })  
          
  output$download_button <- downloadHandler(
    filename = function()
      paste0("Results_",Sys.Date(),".txt"),
    content  = function(file)
    {
      input
      input$send_data
      values$tbl_measures_selected
      values$tbl_metadata_selected
      values$tbl_summaries_tidy
      values$tbl_pairs_calc
    }  
  )

  # observe({
  #   if (input$submit>0)
  #     removeClass(id = "download_button", class = "disabled")
  # })


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