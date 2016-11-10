
x2 <- pi * 100^(-1:3)
round(x2, 3)
signif(x2, digits = 9)

format

c("s","s","G","G","G","s"),align=c("r","r","r","r","r","c")



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
      tbl_metadata,1,
      function(row) 
      {
        browser()
        tags$td(row$parameter_name)
      }
        
    ) 
    
    
      {
        tags$tr(
          
          ),
          # tags$td(
          #   {
          #     if (row$datatype == "TEXT")
          #     {
          #       selectizeInput(
          #         inputId = row$parameter_name,
          #         label = NULL,
          #         selected = row$default_value,
          #         choices = {row$input_options %>% {setNames(.,.)}},
          #         options = list(create = TRUE))
          #     } else
          #       if (row$datatype == "INT")
          #       {
          #         numericInput(
          #           inputId = row$parameter_name,
          #           label = NULL,
          #           step = 1,
          #           value = row$default_value,
          #           min = row$input_min,
          #           max = row$input_max)
          #       } else
          #         if (row$datatype == "FLOAT")
          #         {
          #           numericInput(
          #             inputId = row$parameter_name,
          #             label = NULL,
          #             value = row$default_value,
          #             min = row$input_min,
          #             max = row$input_max)
          #         }
          #   }
          #   
          # ),
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