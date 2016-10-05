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
      title = "App",
      useShinyjs(),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
      tags$head(includeScript("google-analytics.js")),
      tags$head(tags$script(src="scroll.js")),
      # actionButton("console","server console"),
      wellPanel(
        fixedRow(
          column(
            width = 6,
            h5(div(class='step',"Introduction:")),
            p(
              "The application takes mouse phenotyping results from different genotypes measured in your laboratory, and estimates how likely they are to be replicated in other laboratories, utilizing the ",
              tags$i("GxL-Adjustment"),
              " method as detailed in our manuscript \"Addressing reproducibility in single-laboratory phenotyping experiments\" (submitted).",
              actionLink(
                inputId = "read_more",
                label = "Read more...",href = "#intro")
            ),
            hr(),
            #actionButton("Example1", "Fill in example data",class="example_btn"), # uiOutput("example_button"),
            h5(
              div(
                class='step',
                "1."
              ),
              strong(
                "Fill in Experiment and procedure details:"
              )
            ),
            # span(
            #   "Our database includes GxL estimates for various mouse phenotyping procedures and thier measures conducted in several experimental conditions.
            #   Use the dropdown menu to select the phenotyping procedure name which match the one you have conducted in your lab. A table with the relevant experimental conditions will be populated. Check that the details match the ones in your experiment or enter new values.
            #   The procedure details follow the ",
            #   a("IMPRESS",href="https://www.mousephenotype.org/impress",target="_blank"),
            #   "Standard operating procedure (SOP)",
            #   "Next, select the phenotipic measure name you are about to analyse. The measure units will be presented."
            # ),
            selectInput(  
              inputId = "procedure_name",
              label = "Procedure name:",
              choices = c("",procedure_name_list)
            ),
            # uiOutput("proc_SOP_link"),
            uiOutput("metadata_input"),# ,style = "overflow-y:scroll; max-height: 600px"),
            uiOutput("series_input"),
            uiOutput("selected_measure_details"),
            
            h5(
              div(
                class='step',
                "2."
              ),
              strong(
                "Phenotypic measure data input:"
              )
            ),
            # span(
            #   "You may enter the measurments values by uploading a comma-delimited(.csv) file with the observations values (before transformation).
            #   Upload a comma-delimited file with one observation in each row, where the first column contains the group name (genotype name) and the second column contains the observation measurment value.
            #   The file should not contain headers. Do not apply transformation on the values in the file. The data will be transformed later according to the indicated transformation for this measure",
            #   "Alternative input method is entering the groups summary statistics. Fill in the groups name (space delimited) in the input box, and then the groups means, standard deviations and number of observations. Note that the means and standard deviations should be calculated after the specified transformation"
            # ),
            
            # radioButtons(
            #   inputId = "input_method",
            #   label = "Input method:",
            #   choices = list("Uploading a file with observations before transformation" = "file",
            #                  "filling group summaries, after transformation" = "summaries")
            # ),
            radioButtons( 
              inputId = "input_method",
              label = "Input method",
              choices = c("File with raw data" = "file", "Group summaries after transformation" = "summ"),
            ),
            uiOutput("file_form"),
            uiOutput("groups_form"),
            tableOutput("file_summaries"),
            uiOutput("groups_summaries"),
            h5(
              div(
                class='step',
                "3."
              ),
              strong(
                "participation in our program"
              )
            ),
            checkboxInput(
              inputId = "checkbox_agrees_share",
              label = span("Agree to contribute your experimental results and conditions to enrich our ",tags$i("GxL interaction variance estimates database")," and help us yield better estimates in future analyses."
              ),
              value = FALSE
            ),
            disabled(
              div(
                class="form-group shiny-input-container",
                tags$label("Email:"),
                tags$input(id = "email",value = "", type="text", class="form-control",
                           style = 'display:inline-block;')
              )
            ),
            div(id = "wrap_submit_data", actionButton("submit", "Calculate comparisons", icon = icon("cog")))
          ),
          column(
            width = 6,
            tableOutput("check"),
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
              column(3,
                     div(
                       class="form-group shiny-input-container",
                       withMathJax(tags$label("\\(\\alpha\\) - level:")),
                       tags$input(id="alpha", type="number", class="form-control", value="0.05", min="0.001", max="0.5", step="0.01",
                                  style = 'display:inline-block; width:70px; min-width:70px')
                     )
              ),
              column(2,
                     checkboxInput(
                       inputId = "fdr_adjust",
                       label = "BH adjust",
                       value = F
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
            dataTableOutput("results_table"),
            dataTableOutput("results_table_bt"),
            hr(),
            tabsetPanel(
              tabPanel(
                title = "Comparisons plot",
                ""
              ),
              tabPanel(
                title = "Boxplots",""
                #plotOutput("cis_plot",width = "80%",height = "500px")
              )
            ),
            hr(),
            downloadButton("download_button","Download Results",class = "disabled")
          )
        )
      ),
      wellPanel(
        fixedRow(
          column(
            12,
            hr(),
            p(
              "This app is a project by the ",
              a(href = "http://www.replicability.tau.ac.il/", target='_blank',"Replicability Research Group"),
              br(),
              "It's construction is supported by the European Research Council (ERC) under the European Community’s 7th Framework Program (FP/2007-2013) and ERC grant agreement (PSARPS-294519).",
              br(),
              "The app was built with ",
              a(href = "http://shiny.rstudio.com/", target="_blank", "Shiny web application framework by Rstudio"),
              " and is hosted in ",
              a(href = "http://www.shinyapps.io/", target="_blank","shinyapps.io"),".",
              br(),
              "Source code is available on github: ",
              a(href = "http://github.com/shay-y/gxl_app/tree/v0.2-prod", target="_blank","shay-y/gxl_app/tree/v0.2-prod"),".",
              br(),
              "For further information or technical support please contact Shay Yaacoby on github or send an email: ",
              a(href="mailto:shay66[at]gmail.com", target="_blank","shay66[at]gmail.com"),"."
            )
          )
        )
      )
    ),
    tabPanel(
      id = "Information",
      title = "Information",
      fixedRow(
        column(
          width = 8,
          offset = 2,
          wellPanel(
            h5(div(class='step',"Introduction:"),id="intro"),
            p(
              tags$ul(
                tags$li(
                  "The application takes mouse phenotyping results from different genotypes measured in your laboratory, and estimates how likely they are to be replicated in other laboratories, utilizing the ",
                  tags$i("GxL-Adjustment"),
                  " method as detailed in our manuscript \"Addressing reproducibility in single-laboratory phenotyping experiments\" (submitted)."
                ),
                tags$li(
                  "The application takes as input summary statistics from your single lab experiment results - means, standard deviations and number of mices of each genotype."
                ),
                tags$li(
                  "The application then calculates ",
                  tags$i("GxL-Adjusted"),
                  " t-tests for the pairwise differences between the genotypes for a specified phenotypic measure."
                ),
                tags$li(
                  "The Replicability Analyser outputs the",
                  tags$i("GxL-Adjusted"),
                  " p-values and confidence intervals of the genotype differences, alongside the standard p-values and confidence intervals."
                ),
                tags$li(
                  "Prior to the input of the experimental results, The experiment, the procedure and the phenotypic measure details should be specified. The app will find the most appropriate estimate of the GxL interaction variance."
                ),
                tags$li(
                  "The ",
                  tags$i("GxL interaction variance estimate "),"(",
                  withMathJax("\\(S^2_{GxL}\\)"),
                  ") is then used in the calculation of the adjusted p-values and confidence intervals."
                ),
                tags$li(
                  "Contributing your experimental results and the procedure details will enrich our database of ",
                  tags$i("GxL interaction variance estimates"),", yielding better estimates for future users."
                )
              )
            ),
            h5(div(class='step',"brief instructions:")),
            p("You can start by running the built-in example. Clicking the 'Example' button will fill experiment, procedure and measure details and corresponding summary values 
              taken from data presented in the article (see above).",
              br(),
              "Click 'Submit' to see the results. At the end of an exploration, hit F5 key to refresh the app."
            ),
            p("In the table under the \"Results\" section will be listed the difference of means estimates between the genotype pairs, and the corresponding p-values and confidence intervals (GxL-Adjusted and unadjusted)"),
            p("Whenever you want to clear the input fields in any of the sections, click the ",
              tags$code("reset"),
              " button next to the section."
            ),
            hr(),
            h4("References:"),
            p(
              "S. H. Richter, J. P. Garner, B. Zipser, L. Lewejohann, N. Sachser, C. Touma, B. Schindler, S. Chourbaji, C. Brandwein, P. Gass, N. van Stipdonk, J. van der Harst, B. Spruijt, V. V?ikar, D. P. Wolfer, H. W?rbel, ",
              strong("Effect of population heterogenization on the reproducibility of mouse behavior: a multi-laboratory study"),em("PLoS One"),
              " 6(1):e16461 (2011). doi: 10.1371/journal.pone.0016461."
            )
          )
        )
      )
    )
  )
)


# data table table-bordered table-condensed
# HTML(""),
# hr(),
# "stats_tbl"
# actionButton("reset_stats_tbl", " Reset",class="ResetBtn",icon = icon("refresh")),
# HTML("<h5><div class='step'>Results:</div> </h5>")
# label(" Comparisons Diagram:"),
# hidden(div(id="dia_plot_h", ggvisOutput("dia_plot"), align = "center",width = "70%")),
# div(imageOutput("dia_plot_ph",width = "auto",height = "auto"), align = "center"),
# label(" Differences Confidence Intervals Plot:"),
# hidden(div(id="ci_plot_h",plotOutput("ci_plot",width = "80%",height = "500px"), align = "center")),
# div(imageOutput("ci_plot_ph",width = "auto",height = "auto"), align = "center"),
# br(),
# p("Download the detailed results in a text file or the table only in comma-separated file:"),
# dataTableOutput("dt_results", width = "80%"),
# radioButtons("dl_type", label = NULL, choices = list("Detailed (.txt file)"="all","Table only (.csv file)"="table")),
# downloadButton("dl_button","Download",class = "disabled")

