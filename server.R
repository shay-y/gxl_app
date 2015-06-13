#devtools::install_github(c("rstudio/shiny-incubator","trestletech/shinyAce"))
library(shiny)
library(dplyr)
library(shinyIncubator)
library(shinyAce)

# constants:
alpha = 0.05
# L = 6,S = 2,
# genotypes_vec <- c("129S1/SvImJ","A/J","AKR/J","BALB/cByJ","BTBR_T/1_tf/tf","C3H/HeJ","C57BL/6J","C57L/J","C58/J","CAST/Ei","DBA/2J","FVB/NJ","MOLF/Ei","NOD/LtJ","NZB/B1NJ","PERA/Ei","PL/J","SJL/J","SM/J","SPRET/Ei","SWR/J","C57BL/6N","DBA/2")
# datasets:
dat <- read.csv("wuerbel_testsmeasures_units_s2int.csv")

read.csv(file="tbl_meta_data_clean.csv") %>% tbl_df() -> tbl_data

tbl_data %>%
  select(proc_id,procedure.name) %>%
  unique() %>%
  transmute(proc_id_name = paste0(proc_id," : ",procedure.name)) ->
  proc_name_list 

shinyServer(function(input, output, session) {
  # Main new ----
  observe({
    
    input$procedure_id_name %>% substr(1,13) -> selected_proc_id
    
    tbl_data %>% filter(proc_id==selected_proc_id,parameter.name!="directions",
                        measurement_type=="researcher-controlled variable") %>%
      select(Parameter_Name=parameter.name,Value=default.value.for.the.procedure.or.s_int_2,Units=units) %>% 
      as.data.frame()-> meta_df 
    meta_df_txt <- paste0(capture.output(print(meta_df,right = F, row.names = FALSE)),collapse="\n")
    
    if (nrow(meta_df)==0)
      updateAceEditor(session,"meta_parameters_window",value = ".",theme="ambiance",mode = "r")
    else
      updateAceEditor(session,"meta_parameters_window",value = meta_df_txt,theme="ambiance",mode = "r")
    
    tbl_data %>% filter(proc_id==selected_proc_id, measurement_type=="phenotypic measure") %>%
      select(parameter.name) -> parameter_names
    
    updateSelectInput(session,"parameter_name", choices = as.character(parameter_names$parameter.name))
    
  })
  
  output$proc_directions <- renderText({
    input$procedure_id_name %>% substr(1,13) -> selected_proc_id
    tbl_data %>% filter(proc_id==selected_proc_id,parameter.name=="directions") %>%
      select(default.value.for.the.procedure.or.s_int_2) %>% as.data.frame()-> address
    if (is.na(address[1,1]))
      NULL
    else
      paste0("<a href='",address[1,1],"' target='_blank'>Procedure directions and details.</a>") })
  
  #   output$Sint2IMPC <- renderText({
  #     input$parameter_name -> selected_para_id
  #     if (is.na(selected_para_id))
  #       NULL
  #     else
  #     {
  #       tbl_data %>% filter(parameter.name==selected_para_id) %>%
  #         select(value = default.value.for.the.procedure.or.s_int_2) -> sint2
  #       paste0("The genotype X lab interaction estimate is : ",round(sint2[1,1],3) )
  #     }
  #     })  
  
  
  # Main functions ----
  
  #When test type changes, change the avalible meassures: 
  observe({
    TT <- input$TestType
    m_choices <- as.list(dat$Meassure[dat$Test==TT])
    # create label for the meassure:
    names(m_choices) <- paste0(dat$Meassure[dat$Test==TT]," [",dat$unit[dat$Test==TT],",",dat$trans[dat$Test==TT],"]")
    updateSelectInput(session,"Measure",choices = m_choices)
  })
  # get Sint2 from tha csv file to specified meassure and test or IMCP pipeline id 
  Sint2 <- reactive({
    dat$s2int[dat$Meassure == input$Measure & dat$Test == input$TestType]
  })
  # layout table to be filled with experiment results : in each line different genotype 
  output$tbl <- renderUI({
    if (is.null(input$Genotypes))
      return ()
    else
    {
      html <- "<table><thead><tr><th>Group</th><th>Mean</th><th>SD</th><th>N</th></tr></thead><tbody>"
      for (g in input$Genotypes)
        html <- paste0(html,"<tr><td>",g,"</td><td><input class='tb' id='",g,".mean' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,".SD' type='number' value='' step = 'any'/></td><td><input class='tb' id='",g,".N' type='number' value='' step='1'/></td></tr>")
      html <- paste0(html,"</tbody></table>")
      return(HTML(html))
    }
  })
  # main Calculations :
  maketable  <- reactive({
    if (input$go==0)
      return()
    isolate({
      means <- matrix(sapply(paste0("input$'",combn(input$Genotypes, 2),".mean'"), function(x)eval(parse(text=x))) ,ncol = 2,byrow = T)
      SD <- matrix(sapply(paste0("input$'",combn(input$Genotypes, 2),".SD'"), function(x)eval(parse(text=x))) ,ncol = 2,byrow = T)
      N <- matrix(sapply(paste0("input$'",combn(input$Genotypes, 2),".N'"), function(x)eval(parse(text=x))) ,ncol = 2,byrow = T)
      D <- means[,1] - means[,2]
      Sp2 <- ((N[,1]-1)*SD[,1]^2+(N[,2]-1)*SD[,2]^2)/(N[,1]+N[,2]-2)
      Tstat1 <- abs(D)/sqrt(Sp2*(1/N[,1]+1/N[,2]))
      Tstat2 <- abs(D)/sqrt(Sp2*(1/N[,1]+1/N[,2])+2*Sint2())
      pv <- 2-2*pt(q = Tstat1,df = N[,1]+N[,2]-2) 
      ni <- 1/ ((Sp2*(1/N[,1]+1/N[,2]))^2/(N[,1]+N[,2]-2)+(2*Sint2())^2/(L-1)/(S-1)) * (Sp2*(1/N[,1]+1/N[,2])+2*Sint2())^2
      rv <- 2-2*pt(q = Tstat2,df = ni) 
      
      t <-  qt(p = 1-alpha/2,df = ni)
      sp <- t*sqrt(Sp2*(1/N[,1]+1/N[,2]))
      sint  <-  t*sqrt(2*Sint2())
      s <- sqrt(sp^2 + sint^2)
      #       if (length(pv)>1)
      #       {
      #         pv <- pv*length(pv)
      #         rv <- rv*length(rv)
      #       }
      pair <- apply(combn(input$Genotypes, 2),2,function(x) paste(x[1],x[2],sep = " : "))
      data.frame(pair = pair,Difference=D,"p-value" = pv,"GxL-adjusted" = rv,
                 m1=means[,1],m2=means[,2],sp=sp,sint=sint,s=s,t=t)
    })
  })
  # Display: Sint2
  output$Sint2 <- renderText({if (length(Sint2())!=0) if (Sint2()!=0) paste0("The Genotype X Lab Interaction Estimate : ", Sint2())})  
  
  # render results table: 
  output$tbl_pv <- renderTable(maketable()[1:4],digits=c(0,0,6,6,6))
  # render download handler: 
  output$dh <- downloadHandler(filename = function() {paste0("Results_",gsub(" ", "_", input$Measure),".csv")},
                               content = function(file) {write.csv(maketable(),file,row.names = FALSE)})
  
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
  
  observe({
    if (input$Re)
    {
      isolate({
        for (g in input$Genotypes)
        {
          updateTextInput(session, inputId = paste0(g,".mean"),value = "")
          updateTextInput(session, inputId = paste0(g,".SD"),value = "")
          updateTextInput(session, inputId = paste0(g,".N"),value = "")
        }
        
        #1       
        s_options <- list()
        updateTextInput(session, "ExperimentName",value = "")
        updateTextInput(session, "LabAddress", ,value = "")
        updateSelectizeInput(session, "Genotypes", choices = Genotypes, 
                             options = list(create = TRUE, maxOptions = 5,
                                            placeholder = 'Select from the list or add one',onInitialize = I('function() { this.setValue(""); }')))
        #2
        updateDateInput(session,inputId =  "date", value = date())
        updateNumericInput(session,inputId =  "Age", value = "")
        updateRadioButtons(session,inputId =  "Sex",selected = "Males")
        updateSelectInput(session, "TestType",selected = "-")
        updateRadioButtons(session, "ArenaS",selected = "rectangle")
        updateNumericInput(session, "AW", value = "")
        updateNumericInput(session, "AL", value = "")
        updateNumericInput(session, "AR", value = "")
        updateNumericInput(session, "AH", value = "")
        updateSelectizeInput(session, "AM", choices = c("disposable floor paper","non-reflective white plastic"),options = list(create = TRUE, placeholder = 'Select a material or add one',onInitialize = I('function() { this.setValue(""); }')))
        updateNumericInput(session, "TL", value = "")
        updateRadioButtons(session, "TM", selected = "video")
        
        updateNumericInput(session,"Eaf",value = "")
        updateNumericInput(session,"Wh",value = "")
        updateSelectizeInput(session,"Ct", choices = c("Macrolon type II","Macrolon type III"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select type or add one',onInitialize = I('function() { this.setValue(""); }')))
        updateNumericInput(session,"Bh",value = "")
        updateNumericInput(session,"Bw",value = "")
        updateSelectizeInput(session,"Pm", choices = c("wood wrapped with cloth tape"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select material or add one',onInitialize = I('function() { this.setValue(""); }')))
        updateNumericInput(session,"Bh",value = "")
        updateNumericInput(session,"Bw",value = "")
        
        
        updateSelectizeInput(session, "TS",  choices = c("EthoVision <3.0",
                                                         "EthoVision 3.0",
                                                         "EthoVision 3.1",
                                                         "EthoVision XT",
                                                         "ANYmaze"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select a system or add one',onInitialize = I('function() { this.setValue(""); }')))
        
        updateRadioButtons(session,"Phase",selected = "light")
        updateNumericInput(session,"TD", value = "")
        
        #3
        
        
        TT <- input$TestType
        m_choices <- as.list(dat$Meassure[dat$Test==TT])
        names(m_choices) <- paste0(dat$Meassure[dat$Test==TT]," [",dat$unit[dat$Test==TT],",",dat$trans[dat$Test==TT],"]")
        updateSelectInput(session,"Measure",choices = m_choices)
        
        
      })
      
    }
    else
      return()
  })
  
  observe({
    if (input$Rt)
    {
      isolate({
        
        for (g in input$Genotypes)
        {
          updateTextInput(session, inputId = paste0(g,".mean"),value = "")
          updateTextInput(session, inputId = paste0(g,".SD"),value = "")
          updateTextInput(session, inputId = paste0(g,".N"),value = "")
        }
        
        #2
        updateDateInput(session,inputId =  "date", value = date())
        updateNumericInput(session,inputId =  "Age", value = "")
        updateRadioButtons(session,inputId =  "Sex",selected = "Males")
        updateSelectInput(session, "TestType",selected = "-")
        updateRadioButtons(session, "ArenaS",selected = "rectangle")
        updateNumericInput(session, "AW", value = "")
        updateNumericInput(session, "AL", value = "")
        updateNumericInput(session, "AR", value = "")
        updateNumericInput(session, "AH", value = "")
        updateSelectizeInput(session, "AM", choices = c("disposable floor paper","non-reflective white plastic"),options = list(create = TRUE, placeholder = 'Select a material or add one',onInitialize = I('function() { this.setValue(""); }')))
        updateNumericInput(session, "TL", value = "")
        updateRadioButtons(session, "TM", selected = "video")
        
        updateNumericInput(session,"Eaf",value = "")
        updateNumericInput(session,"Wh",value = "")
        updateSelectizeInput(session,"Ct", choices = c("Macrolon type II","Macrolon type III"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select type or add one',onInitialize = I('function() { this.setValue(""); }')))
        updateNumericInput(session,"Bh",value = "")
        updateNumericInput(session,"Bw",value = "")
        updateSelectizeInput(session,"Pm", choices = c("wood wrapped with cloth tape"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select material or add one',onInitialize = I('function() { this.setValue(""); }')))
        updateNumericInput(session,"Bh",value = "")
        updateNumericInput(session,"Bw",value = "")
        
        
        updateSelectizeInput(session, "TS",  choices = c("EthoVision <3.0",
                                                         "EthoVision 3.0",
                                                         "EthoVision 3.1",
                                                         "EthoVision XT",
                                                         "ANYmaze"),options = list(create = TRUE,maxOptions = 5, placeholder = 'Select a system or add one',onInitialize = I('function() { this.setValue(""); }')))
        
        updateRadioButtons(session,"Phase",selected = "light")
        updateNumericInput(session,"TD", value = "")
        
        #3
        
        
        TT <- input$TestType
        m_choices <- as.list(dat$Meassure[dat$Test==TT])
        names(m_choices) <- paste0(dat$Meassure[dat$Test==TT]," [",dat$unit[dat$Test==TT],",",dat$trans[dat$Test==TT],"]")
        updateSelectInput(session,"Measure",choices = m_choices)
        
        
      })
      
    }
    else
      return()
  })
  
  observe({
    if (input$Rm)
    {
      isolate({
        
        for (g in input$Genotypes)
        {
          updateTextInput(session, inputId = paste0(g,".mean"),value = "")
          updateTextInput(session, inputId = paste0(g,".SD"),value = "")
          updateTextInput(session, inputId = paste0(g,".N"),value = "")
        }
        
        #3
        
        
        TT <- input$TestType
        m_choices <- as.list(dat$Meassure[dat$Test==TT])
        names(m_choices) <- paste0(dat$Meassure[dat$Test==TT]," [",dat$unit[dat$Test==TT],",",dat$trans[dat$Test==TT],"]")
        updateSelectInput(session,"Measure",choices = m_choices)
        
        
        
      })
      
    }
    else
      return()
  })
  
  observe({
    if (input$Rr)
    {
      isolate({
        
        for (g in input$Genotypes)
        {
          updateTextInput(session, inputId = paste0(g,".mean"),value = "")
          updateTextInput(session, inputId = paste0(g,".SD"),value = "")
          updateTextInput(session, inputId = paste0(g,".N"),value = "")
        }
        
      })
      
    }
    else
      return()
  })
  
  observe({
    input$Measure
    for (g in input$Genotypes)
    {
      updateTextInput(session, inputId = paste0(g,".mean"),value = "")
      updateTextInput(session, inputId = paste0(g,".SD"),value = "")
      updateTextInput(session, inputId = paste0(g,".N"),value = "")
    }
  })
  # Examples ----------------------------------------------------------------
  
  ## Example 1:
  
  observe({
    if (input$Example1a)
    {
      isolate({
        
        updateTextInput(session, "ExperimentName",value = "Example_1_OFT")
        updateTextInput(session, "LabAddress", ,value = "Utrecht")
        updateSelectizeInput(session, "Genotypes", selected = c("C57BL/6N","DBA/2"))
        
        updateDateInput(session,inputId =  "date", value = "2011-01-01")
        updateNumericInput(session,inputId =  "Age", value = "12")
        updateRadioButtons(session,inputId =  "Sex",selected = "Females")
        updateSelectInput(session, "TestType",selected = "Open field test")
        updateRadioButtons(session, "ArenaS",selected = "rectangle")
        updateNumericInput(session, "AW", value = 50)
        updateNumericInput(session, "AL", value = 50)
        updateNumericInput(session, "AR", value = "")
        updateNumericInput(session, "AH", value = 37)
        updateSelectizeInput(session, "AM", selected = "disposable floor paper")
        updateNumericInput(session, "TL", value = 60)
        updateRadioButtons(session, "TM", selected = "video")
        updateSelectizeInput(session, "TS",  selected = c("EthoVision 3.1"))
        
        updateRadioButtons(session,"Phase",selected = "light")
        updateNumericInput(session,"TD", value = 10)
        updateSelectInput(session,inputId = "Measure",selected = "bolus count")
        
      })
    }
    else
      return()
  })
  
  observe({
    if (input$Example1b)
    {
      isolate({
        updateTextInput(session, inputId = "C57BL/6N.mean",value = 4.438)
        updateTextInput(session, inputId = "C57BL/6N.SD",value = 3.204)
        updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
        updateTextInput(session, inputId = "DBA/2.mean",value = 10.938)
        updateTextInput(session, inputId = "DBA/2.SD",value = 4.074)
        updateTextInput(session, inputId = "DBA/2.N",value = 16)
      })
    }
    else
      return()
    
  })
  
  
  ## Example 2:
  
  observe({
    if (input$Example2a)
    {
      isolate({
        
        updateTextInput(session, "ExperimentName",value = "Example_2_OFT")
        updateTextInput(session, "LabAddress", ,value = "Giessen")
        updateSelectizeInput(session, "Genotypes", selected = c("C57BL/6N","DBA/2"))
        
        updateDateInput(session,inputId =  "date", value = "2011-01-01")
        updateNumericInput(session,inputId =  "Age", value = "12")
        updateRadioButtons(session,inputId =  "Sex",selected = "Females")
        updateSelectInput(session, "TestType",selected = "Open field test")
        updateRadioButtons(session, "ArenaS",selected = "rectangle")
        updateNumericInput(session, "AW", value = 50)
        updateNumericInput(session, "AL", value = 50)
        updateNumericInput(session, "AR", value = "")
        updateNumericInput(session, "AH", value = 37)
        updateSelectizeInput(session, "AM", selected = "disposable floor paper")
        updateNumericInput(session, "TL", value = 60)
        updateRadioButtons(session, "TM", selected = "video")
        updateSelectizeInput(session, "TS",  selected = c("EthoVision 3.1"))
        
        updateRadioButtons(session,"Phase",selected = "light")
        updateNumericInput(session,"TD", value = 10)
        updateSelectInput(session,inputId = "Measure",selected = "total path moved")
        
      })
    }
    else
      return()
  })
  
  observe({
    if (input$Example2b)
    {
      isolate({
        updateTextInput(session, inputId = "C57BL/6N.mean",value = 59.861)
        updateTextInput(session, inputId = "C57BL/6N.SD",value = 5.977)
        updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
        updateTextInput(session, inputId = "DBA/2.mean",value = 70.768)
        updateTextInput(session, inputId = "DBA/2.SD",value = 14.279)
        updateTextInput(session, inputId = "DBA/2.N",value = 16)
      })
    }
    else
      return()
    
  })
  
  ## Example 3:
  
  observe({
    if (input$Example3a)
    {
      isolate({
        
        updateTextInput(session, "ExperimentName",value = "Example_3_OFT_multi")
        updateTextInput(session, "LabAddress", ,value = "Giessen")
        updateSelectizeInput(session, "Genotypes", selected = c("C57BL/6N","DBA/2"))
        
        updateDateInput(session,inputId =  "date", value = "2011-01-01")
        updateNumericInput(session,inputId =  "Age", value = "12")
        updateRadioButtons(session,inputId =  "Sex",selected = "Females")
        updateSelectInput(session, "TestType",selected = "Open field test")
        updateRadioButtons(session, "ArenaS",selected = "rectangle")
        updateNumericInput(session, "AW", value = 50)
        updateNumericInput(session, "AL", value = 50)
        updateNumericInput(session, "AR", value = "")
        updateNumericInput(session, "AH", value = 37)
        updateSelectizeInput(session, "AM", selected = "disposable floor paper")
        updateNumericInput(session, "TL", value = 60)
        updateRadioButtons(session, "TM", selected = "video")
        updateSelectizeInput(session, "TS",  selected = c("EthoVision 3.1"))
        
        updateRadioButtons(session,"Phase",selected = "light")
        updateNumericInput(session,"TD", value = 10)
        updateSelectInput(session,inputId = "Measure",selected = "path moved within centre")
        
      })
    }
    else
      return()
  })
  
  observe({
    if (input$Example3b)
    {
      isolate({
        updateTextInput(session, inputId = "C57BL/6N.mean",value = 49.513)
        updateTextInput(session, inputId = "C57BL/6N.SD",value = 6.411)
        updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
        updateTextInput(session, inputId = "DBA/2.mean",value = 56.814)
        updateTextInput(session, inputId = "DBA/2.SD",value = 17.330)
        updateTextInput(session, inputId = "DBA/2.N",value = 16)
      })
    }
    else
      return()
    
  })
  
  
  ## Example 4:
  
  observe({
    if (input$Example4a)
    {
      isolate({
        
        updateTextInput(session, "ExperimentName",value = "Example_4_NOT")
        updateTextInput(session, "LabAddress", ,value = "Mannhein")
        updateSelectizeInput(session, "Genotypes", selected = c("C57BL/6N","DBA/2"))
        
        updateDateInput(session,inputId =  "date", value = "2012-02-02")
        updateNumericInput(session,inputId =  "Age", value = "12")
        updateRadioButtons(session,inputId =  "Sex",selected = "Females")
        updateSelectInput(session, "TestType",selected = "Novel object test")
        updateNumericInput(session, "TL", value = 60)
        updateRadioButtons(session, "TM", selected = "video")
        updateSelectizeInput(session, "TS",  selected = c("EthoVision 3.1"))
        
        updateRadioButtons(session,"Phase",selected = "light")
        updateNumericInput(session,"TD", value = 10)
        updateSelectInput(session,inputId = "Measure",selected = "path moved within exploration zone 1")
        updateSelectInput(session,inputId = "Measure",selected = "path moved within exploration zone 1")
        
      })
      
    }
    else
      return()
  })
  
  observe({
    if (input$Example4b)
    {
      isolate({
        updateTextInput(session, inputId = "C57BL/6N.mean",value = 15.544)
        updateTextInput(session, inputId = "C57BL/6N.SD",value = 5.192)
        updateTextInput(session, inputId = "C57BL/6N.N",value = 16)
        updateTextInput(session, inputId = "DBA/2.mean",value = 20.346)
        updateTextInput(session, inputId = "DBA/2.SD",value = 7.783)
        updateTextInput(session, inputId = "DBA/2.N",value = 16)
      })
    }
    else
      return()
  })
  
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



