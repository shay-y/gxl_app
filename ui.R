shinyUI(
  
  navbarPage(
    id ="nav",
    # theme = "theme.css",
    title = div(
      span(id = "title", "GxL Replicability Adjuster "),
      span(" - A tool for genotype lab replicability assessment of single lab mouse phenotyping results")),
    windowTitle = "GxL Replicabilty Adjuster",
    fluid = F,
    tabPanel(
      title = NULL,
      # actionButton("console","server console"),
      # runcodeUI(),
      tagList(
        useShinyjs(),
        tags$head(
          # includeCSS("https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.min.css"),
          includeCSS("WWW/style.css"),
          includeScript("google-analytics.js"),
          includeScript("WWW/scroll.js")
        ),
        wellPanel(
          fixedRow(
            column(
              width = 6,
              h5(div(class='step',"Introduction:")),
              p(
                "This application takes mouse phenotyping results measured in your laboratory, comparing two or more groups, and adjusts the p-values and confidence intervals to reflect how they are likely to be, had your experiment been reproduced in other laboratories. The app utilizes the ",
                tags$i("GxL-Adjustment")," method for the pairwise t-tests, as detailed in our manuscript ",
                tags$i("Addressing reproducibility in single-laboratory phenotyping experiments"),
                " (submitted). ",
                actionLink(
                  inputId = "read_more",
                  label = "Read more...",href = "#intro")
              ),
              hr(),
              # htmlTemplate("examples_dropdown.html"),
              actionButton(inputId = "reset_all",label = "Reset", icon = icon("refresh"),class = "btn_right btn-sm"),
              actionButton(inputId = "load_example_button",label = "Load example", class = "btn_right btn-sm"),
              h5(
                div(
                  class='step',
                  "1."
                ),
                strong(
                  "Experiment details:"
                ),
                popify(
                  el = icon("info-circle"),
                  title = NULL,
                  content = withTags(tagList(
                    ul(
                      li("The GxL adjustment is tailored to the procedure and then, depending on the procedure, to the phenotypic measure being used."),
                      li("Select the procedure, then fill in the procedure parameters (using the dropdown menus in the table that opens after the procedure selection)."),
                      li("If a value for for a procedure parameter is missing in the dropdown menu - fill it manually."),
                      li("If the procedure or the measure is not specified yet - give it a name of your choice.",
                         br(),
                         "In this case the application will output unadjusted results only (and will enrich the database for future use)"
                      )
                    )
                  ))%>%  str_replace_all(pattern = "\n",replacement = ""),
                  placement = "right",
                  trigger = c("hover","focus"),
                  options = NULL)
              ),
              selectInput(  
                inputId = "procedure_name",
                label = "Procedure name:",
                choices = c("",procedure_name_list)
              ),
              uiOutput("metadata_input"),# ,style = "overflow-y:scroll; max-height: 600px"),
              h5(
                div(
                  class='step',
                  "2."
                ),
                strong(
                  "Data input for the selected phenotypic measure:"
                )
              ),
              tabsetPanel(
                id="input_method",
                tabPanel(
                  title = 
                    tagList(
                      "Option 1:Upload a .csv file",
                      popify(
                        el = icon("info-circle"),
                        title = NULL,
                        content = 
                          withTags(tagList(
                            ul(
                              li(
                                "Upload a comma-delimited file (without headers) with one observation in each row where the first column contains the group name and the second column contains the measurement."
                              ),
                              li(
                                "You may use group names of your choice, and these names will appear on the results tables and figures."
                              ),
                              li(
                                b("Do not apply any transformation")," on the values in the file. The data will be transformed according to the specified transformation for the measure you have selected in step 1."
                              ),
                              li(
                                p(b("Part of example table:")),
                                div(
                                  table( 
                                    class= "table table-striped table-bordered table-condensed",
                                    tbody(
                                      tr(td("C57BL/6"),td("2145.6")),
                                      tr(td("C57BL/6"),td("1941.75")),
                                      tr(td("C57BL/6"),td("1428.9")),
                                      tr(td("Elk4"),td("1941.3")),
                                      tr(td("Elk4"),td("1528.125"))
                                    )
                                  )
                                ),
                                p(b("The corresponding part in the comma-delimited file:")),
                                div(
                                  style="font-family:\'Courier New\', Courier, monospace;font-size:14px",
                                  "C57BL/6,2145.6",br(),
                                  "C57BL/6,1941.75",br(),
                                  "C57BL/6,1428.9",br(),
                                  "C57BL/6,1947.52",br(),
                                  "Elk4,1941.3",br(),
                                  "Elk4,1528.125",br(),
                                  "Elk4,1404.15",br()
                                )
                              )
                            )
                          )
                          )%>%  str_replace_all(pattern = "\n",replacement = ""),
                        placement = "right",
                        trigger = c("hover","focus"),
                        options = NULL
                      )
                    ),
                  value = "file",
                  # b("Upload a file with observations in each row (before transformation)"),
                  uiOutput("file_form")
                ),
                tabPanel(
                  title = 
                    tagList(
                      "Option 2: Fill in the groups summaries",
                      popify(
                        el = icon("info-circle"),
                        title = NULL,
                        content = withTags(
                          tagList(
                            ul(
                              li("Select the number of groups in the experiment."),
                              li("Fill in the groups names, means, standard deviations and number of observations."),
                              li("For group names, you may select from the dropdown list or type-in a name of your choise"),
                              li("The means and standard deviations should be calculated ", b("after applying")," the specified transformation for the selected measure.")
                            )
                          )) %>%  str_replace_all(pattern = "\n",replacement = ""),    
                        placement = "right",
                        trigger = c("hover","focus"),
                        options = NULL)
                    ),
                  value = "summ",
                  div(
                    class="form-group shiny-input-container",
                    tags$label("Number of groups: "),
                    tags$input(id="n_group_inputs", type="number", class="form-control", value="2", min="2", max="8", step="1",
                               style = 'display:inline-block; width:50px; min-width:50px')
                  ),
                  uiOutput("groups_form")
                )
              )
              ,
              h5(
                div(
                  class='step',
                  "3."
                ),
                strong(
                  "Participation in our program:"
                )
              ),
              checkboxInput(
                inputId = "agree_contribute",
                label = span("I agree to contribute the experimental results and testing conditions to enrich the ",tags$i("GxL estimates database")," and help yield better estimates for future users."
                ),
                value = FALSE
              ),
              hidden(
                div(
                  id = "user_details",
                  tags$form(
                    class = "form-inline",
                    div(
                      class="form-group shiny-input-container",
                      tags$label("Lab name:"),
                      tags$input(id = "lab",value = "", type="text", class="form-control")
                    ),
                    div(
                      class="form-group shiny-input-container",
                      tags$label("Your email:"),
                      tags$input(id = "email",value = "", type="text", class="form-control")
                    )
                  ),
                  uiOutput("groups_info"),
                  uiOutput("info_submit_feedback"),
                  div(
                    id = "wrap_submit_data",
                    actionButton(
                      "submit_data",
                      "Submit data",
                      class= "btn_right" #,icon = icon("cog")
                    ) 
                  )
                )
              )
            ),
            column(
              width = 6,
              h5(
                div(
                  class='step',
                  " "
                ),
                strong(
                  "Analysis options"
                )
              ),
              fixedRow(
                column(
                  6,
                  div(
                    class="form-group shiny-input-container",
                    tags$label(
                      "Confidence level ",
                      withMathJax(("\\((1-\\alpha)\\)")),
                      # popify(
                      #   el = icon("info-circle"),
                      #   title = NULL,
                      #   content = 
                      #     "...",
                      #   placement = "left",
                      #   trigger = "hover",
                      #   options = NULL),
                      ":"),
                    tags$input(id="conf_level", type="number", class="form-control", value="0.95", min="0.001", max="0.5", step="0.01",
                               style = 'display:inline-block; width:70px; min-width:70px')
                  ) #,checkboxInput("back_transform", "Transform the estimates in the table back to the original scale", FALSE)
                ),
                column(
                  6,
                  radioButtons(
                    inputId = "mult_adjust",
                    label = 
                      tagList(
                        "Pairwise comparisons adjustment:"
                        # ,popify(
                        #   el = icon("info-circle"),
                        #   title = NULL,
                        #   content = 
                        #     "...",
                        #   placement = "left",
                        #   trigger = "hover",
                        #   options = NULL)
                      ),
                    choices = c("FDR (Benjamini-Hochberg)"="BH selected", "Tukey HSD"="Tukey HSD", "None" = "none"),
                    inline = F
                  )
                )
              ),
              h5(
                div(
                  class='step',
                  "Results"
                ),
                strong(
                  ""
                )
              ),
              uiOutput("measure_selected_details"),
              dataTableOutput("file_summaries_table"),
              dataTableOutput("pairs_table"),
              hr(),
              tabsetPanel(
                id = "plots_tabset",
                tabPanel(
                  title = "Boxplots",
                  plotOutput("box_plot"),
                  plotOutput("box_plot_bt")
                ),
                tabPanel(
                  title = "Comparisons plot",
                  plotOutput("pcci_plot",height = "650px")
                )
              )
            )
          )
        ),
        wellPanel(
          fixedRow(
            column(
              12,
              p(
                "The application is a project by the ",
                a(href = "http://www.replicability.tau.ac.il/", target='_blank',"Replicability Research Group"),".",
                br(),
                "The research leading to the GxL adjustment and the development of the application are supported by the European Research Council (ERC) under the European Community’s 7th Framework Program (FP/2007-2013) grant agreement (PSARPS-294519).",              br(),
                "The application wass built with ",
                a(href = "http://shiny.rstudio.com/", target="_blank", "Shiny web application framework by Rstudio"),".",
                br(),
                "Source code is available on Github: ",
                a(href = "http://github.com/shay-y/gxl_app/", target="_blank","shay-y/gxl_app/"),".",
                br(),
                "For technical support and comments contact Shay Yaacoby at ",
                a(href="mailto:shay66[at]gmail.com", target="_blank","shay66[at]gmail.com"),"."
              )
            )
          )
        ),
        bsModal(
          id = "Information",
          title = NULL,
          size = "large",
          trigger = "read_more",
          # fixedRow(
          #   column(
          #     width = 8,
          #     offset = 2,
          #     wellPanel(
          h5(div(class='step',"Introduction:"),id="intro"),
          p(
            tags$ul(
              tags$li(
                "This application takes mouse phenotyping results measured in your laboratory, comparing two or more groups, and adjusts the p-values and confidence intervals to reflect how they are likely to be, had your experiment been reproduced in other laboratories, utilizing the GxL-adjustment method as detailed in our manuscript Addressing reproducibility in single-laboratory phenotyping experiments(submitted)."
              ),
              tags$li(
                "The application takes as input either:",
                tags$ul(
                  tags$li("Comma-delimited file with all observations, or"),
                  tags$li("Keyed-in summary statistics for each of the groups: means, standard deviations and number of mices.")
                )
              ),
              tags$li(
                "The application then calculates GxL-adjusted t-tests for the pairwise differences between the groups for the specified phenotypic measure. It outputs the GxL-adjusted p-values and confidence intervals of the differences, alongside the standard p-values and confidence intervals."
              ),
              tags$li(
                "Prior to the input of the data, the user should specify the details for the experimental procedure and the phenotypic measure. The application will generate the most appropriate GxL adjustment ratio."
              ),
              tags$li(
                "The GxL adjustment ratio is then used in the calculation of the p-values and confidence intervals."
              ),
              tags$li(
                "Contributing your experimental results and the procedure details will enrich the GxL estimates database, yielding better estimates for future users."
              )
            )
          )
        )
      )
    )
  )
)
