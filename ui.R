shinyUI(
  navbarPage(title = HTML("Genotype-Lab Replicability Analyzer"),fluid = T,
             tabPanel("App",
                      useShinyjs(),
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
                      fixedRow(
                        # actionButton("console","server console"),
                        # h2("Genotype-Lab Replicability Analyzer",style = 'font-family: "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif;font-weight: 300;'),
                        # h4("A tool for replicability assessment of mouse phenotyping results across laboratories",style = 'font-family: "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif;font-weight: 300;'),
                        actionButton("Example2", "Example 2",class="ResetBtn",icon = icon("refresh")),
                        actionButton("Example1", "Example 1",class="ResetBtn",icon = icon("refresh"))
                        # actionButton("Example1_step1", "Example 4 param.",class="ResetBtn",icon = icon("refresh"))
                      ),
                      fixedRow(
                        column(
                          4,
                          
                          HTML("<h5><div class='step'>Step 1:</div> Fill in the experiment level details:</h5>"),
                          
                          wellPanel(
                            #selectizeInput("lab_name",'Lab name: ', choices = lab_names_vec, multiple = F, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list or add one',onInitialize = I('function() { this.setValue(""); }'))),
                            textInput("lab_name", "Lab name:",value = "your_lab_name"), # Fill in lab name. It will appear in the results' file name.
                            radioButtons("expr_design",'Comparisons design:', choices = list("All genotypes pairs" = "Tukey","Many genotypes to one (Cases vs. control)" = "Dunnet"),selected = "Tukey",inline = T),# You may select to compare 1) each genotype to any other genotype or 2) one "control genotype to any other genotype."
                            uiOutput("genotype_selection_block"),
                            radioButtons("proc_gender",'Gender:', choices = c("Males","Females","Males & Females"),inline = T),
                            hidden(textInput("experiment_identifier", "Experiment identifier:",value = "")),
                            actionButton("reset_experiment", " Reset",class="ResetBtn",icon = icon("refresh")),
                            HTML("<div style='clear: both;'></div>")
                          ),
                          
                          HTML("<h5><div class='step'>Step 2:</div> Select procedure (test), verify procedure's details in the link to Standard Operating Procedure and fill-in any changed meta-data parameters:</h5>"),
                          
                          wellPanel(
                            selectizeInput("proc_name",'Procedure Name:',
                                           choices = proc_name_list, #setNames(nm = as.character(unique(dat$Test)[c(7,4,1,2,3,5,6,8:(8+33))]),as.list(as.character(unique(dat$Test)[c(7,4,1,2,3,5,6,8:(8+33))])))),
                                           options = list(onInitialize = I('function() { this.setValue(""); }'))),
                            # HTML("Choose phenotypic assay \\ Standard Operating Procedure(SOP) \\ phenotypic test:"),
                            htmlOutput("proc_SOP_link"),
                            hr(),
                            p(strong("Researcher-Controlled Variables"),"Following are the default researcher-controlled variables for the procedure. Correct and\\or add values as used in your experiment. If value is unknown or irrelevant, fill in 'NA' :"),
                            aceEditor("meta_data_editor",".",theme="ambiance",mode = "r",height = "200px",fontSize = 15),
                            #                             numericInput("proc_age", "Age at testing [weeks]:", ""),
                            #                             numericInput("proc_duration", "Procedure duration [min]:", ""),
                            #                             HTML(
                            #                               "<div class='form-group shiny-input-container'>
                            #                               <label for='proc_notes'>Procedure notes:</label>
                            #                               <textarea id='proc_notes' class='form-control' rows='2' cols='50' value=''></textarea>
                            #                               </div> "
                            #                             ),
                            actionButton("reset_proc", " Reset",class="ResetBtn",icon = icon("refresh")),
                            HTML("<div style='clear: both;'></div>")
                            #actionButton(inputId = "submit_editor",label = "Submit"),
                          )
                        ),
                        column(
                          4,
                          HTML("<h5><div class='step'>Step 3:</div> Select phenotypic measure\\s (note the meassuring unit and transformation):</h5>"),
                          
                          wellPanel(
                            selectizeInput("measure_name",'Phenotypic Measure: ',
                                           choices = measure_name_list ,#  setNames(nm = paste0(dat$Meassure[1],"[",dat$unit[1],",",dat$trans[1],"]"),object = as.list(paste0(dat$Meassure[1],"[",dat$unit[1],",",dat$trans[1],"]"))),width = "auto"),
                                           options = list(onInitialize = I('function() { this.setValue(""); }'))),
                            htmlOutput("selected_measure_details"),     
                            actionButton("reset_measure", " Reset",class="ResetBtn",icon = icon("refresh")),
                            HTML("<div style='clear: both;'></div>")
                          ),
                          
                          HTML("<h5><div class='step'>Step 4:</div> Fill in the resulted statistics from your experiment in the table:</h5>"),
                          
                          wellPanel(
                            # textOutput("ctrl"),
                            conditionalPanel(
                              condition = 'values.genotypes_tested != NULL',
                              htmlOutput("stats_tbl") # stats table for each meassure, add note about multiplicity
                            ),
                            actionButton("reset_stats_tbl", " Reset",class="ResetBtn",icon = icon("refresh")),
                            HTML("<div style='clear: both;'></div>")
                          ),
                          
                          HTML("<h5><div class='step'>Step 5:</div></h5>"),
                          
                          wellPanel(
                            withMathJax(HTML("<div class='form-group shiny-input-container'>
                                             <label for='alpha'>\\(\\alpha\\) - level:</label>
                                             <input id='alpha' type='number' class='form-control' value='0.05' min='0' max='0.5' step='0.01',style='display:inline-block; width:100px; min-width:100px'/></div>")),
                            checkboxInput("mult_correct","Apply multiplicity correction"),
                            checkboxInput("checkbox_agrees_share","Contribute the project by sending the experiment settings and results to our database."), # improve
                            div(id = "wrap_submit_data", actionButton("submit_data", "Submit", icon = icon("cog")))
                            ),
                          HTML("<h5><div class='step'>Results:</div> </h5>"),
                          wellPanel(
                              withTags(
                                div(id = "results_sec",
                                    label(" Summary Table:"),
                                    DT::dataTableOutput("out_tbl", width = "80%"),
                                    br(),
                                    label(" Comparisons Diagram:"),
                                    hidden(div(id="dia_plot_h", ggvisOutput("dia_plot"), align = "center",width = "70%")),
                                    div(imageOutput("dia_plot_ph",width = "auto",height = "auto"), align = "center"),
                                    label(" Differences Confidence Intervals Plot:"),
                                    hidden(div(id="ci_plot_h",plotOutput("ci_plot",width = "80%",height = "500px"), align = "center")),
                                    div(imageOutput("ci_plot_ph",width = "auto",height = "auto"), align = "center"),
                                    br(),
                                    p("Download the detailed results in a text file or the table only in comma-separated file:"),
                                    radioButtons("dl_type", label = NULL, choices = list("Detailed (.txt file)"="all","Table only (.csv file)"="table")),
                                    downloadButton("dl_button","Download",class = "disabled")
                                )
                              )
                              )
                          )
                        ),
                      fixedRow(
                        column(
                          8,offset = 2,
                          hr(),
                          wellPanel(
                            p(
                              HTML("The <strong><em>Genotype-Lab Replicability Analyzer</em></strong> is a project by the <a href = \"http://www.replicability.tau.ac.il/\" target='_blank'>Replicability Research Group</a>. <br>
It's construction is supported by the European Research Council (ERC) under the European Community’s 7th Framework Program (FP/2007-2013) and ERC grant agreement (PSARPS-294519).<br>
<br>
The application is built with <a href = \"http://shiny.rstudio.com/\" target=\"_blank\">Shiny</a> web application framework and is hosted in <a href = \"http://www.shinyapps.io/\" target=\"_blank\">shinyapps.io</a> . The source code will be avaliable in github soon.
<br>
For further information and technical support please contact Shay Yaacoby at : <a href=\"mailto:shay66[at]gmail.com\" target=\"_blank\">shay66[at]gmail.com</a>.
"
                                )
                              )
                          )
                          ) # end column
                        ) # end row
                      ), # tabPanel

             tabPanel("Introduction",
                      fixedRow(
                        column(8,offset = 2,
                               h3("Introduction"),
                               p("This application takes mouse phenotyping results measured in your laboratory, and estimates how likely they are to be replicated in other laboratories, using the Random Lab Model with the GxL-adjusting method, as detailed in our manuscript \"Assessing the replicability of single-laboratory discoveries in phenotyping experiments\" (submitted)."),
                               p("The application takes as input single-lab phenotyping results from animals belonging to several genotype groups (for example: knockouts vs wildtype controls). The method outputs the p-value of the difference between the groups, using the standard t-test or ANOVA, and the adjusted pvalue using previously estimated Genotype-by-Laboratory interaction (GxL). This replicability across laboratory p-value is the more reliable p-value for assessing the replicability of any single-lab genotype effect."),
                               p("Knowing the relevant GxL is the difficult part. The application is therefore limited to phenotypic tests, phenotyping measures and testing conditions for which we can currently us by contributing your phenotyping results and specifying the housing and testing conditions in your laboratory. These will be used to calculate a better estimate of GxL variability, based on more genotypes, more laboratories and more conditions, that will serve you and other researchers over the world in ensuring the replicability of their future phenotyping results. You also have the option not to contribute your results, for example if you do not trust them, or if you are only testing the application with hypothetical values."),
                               h3("Instructions"),
                               h4("Example"),
                               p("You can start by running a built-in example:"),
                               tags$ol(
                                 tags$li("Click \"Example 1\" or \"Example 2\" (on the upper right) to fill test properties and measurments."),
                                 tags$li("Click \"Submit\"."),
                                 tags$li("At the end of exploration, hit F5 key to refresh the app.")
                               ),
                               p("Under “Results” you will get the difference between the genotype means, the p-value of the genotype effect using standard t-test (or ANOVA if there’s more than 2 genotypes), and the the adjusted p-value for the Genotype x Lab (GxL) interaction."),
                               p("Whenever you want to clear any of the sections, click the <code>reset</code> button next to this section. You can insert your own data."),
                               hr(),
                               h4("References"),
                               p(HTML("S. H. Richter, J. P. Garner, B. Zipser, L. Lewejohann, N. Sachser, C. Touma, B. Schindler, S. Chourbaji, C. Brandwein, P. Gass, N. van Stipdonk, J. van der Harst, B. Spruijt, V. V?ikar, D. P. Wolfer, H. W?rbel, <strong>Effect of population heterogenization on the reproducibility of mouse behavior: a multi-laboratory study.</strong> <em>PLoS One</em> 6(1):e16461 (2011). doi: 10.1371/journal.pone.0016461."))
                        )
                      )
             )
  )
)
# conditionalPanel(condition = "input.expr_design == 'Tukey'",
#                  selectizeInput("genotypes_tested_pairwise",'Genotypes tested:', choices = genotypes_vec, multiple = TRUE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
# ),
# conditionalPanel(condition = "input.expr_design == 'Dunnet'",
#                  selectizeInput("genotypes_tested_control",'Genotype tested (control group):', choices = genotypes_vec, multiple = FALSE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list one',onInitialize = I('function() { this.setValue(""); }'))),
#                  selectizeInput("genotypes_tested_cases",'Genotypes tested (cases groups):', choices = genotypes_vec, multiple = TRUE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
# ),
#                         The example currently uses existing data for the Open Field Test (OFT), the phenotypic measure of “total path moved”, and the genotypes C57BL6NCrl and DBA/2NCrl.</p>
#                         insert a Name or Identifier ,the Lab Address and the genotypes. Choose at least two genotypes for the comparison. More that two genotypes can be entered and all pairwise comparisons will be calculated.  <br>
#                           In <strong>section 2</strong> choose first in “Test Type” the phenotypic test you used to measure your results. The application currently has a selection of 6 phenotypic tests. The application cannot be used for a test missing from the list. <br>
#                           Once the test is chosen, the windows for the test conditions become available below it. Insert the values used in your study. <br>
#                           In <strong>section 3</strong> choose the phenotypic measure. Every Test Type has a list of at least one available measures. The application cannot be used for a measure missing from the list. <br>
#                           In <strong>section 4</strong> enter the test results, group mean, group standard deviation and group size for each genotype.</p>
#                           <h4 id='open-field-test'>Open field test</h4>
#                           <p>The Open Field Test (OFT) is probably the most frequently used of all behavioral tests in pharmacology and neuroscience. However, there is a wide variety of OFT procedures using different arena shapes and sizes, tracking technologies, tracking systems, session lengths and many specific parameters which might affect the results. Our available data currently represent Richter et al. 2011, which describe its methods as follows: <br>
#                           The apparatus consisted of an open box, 40 cm x 40 cm minimum size, virtually divided into various zones (corners, 5 cm wall zone, centre). Mice were placed into the centre of the empty open field arena and video tracked for 10 min. The time spent in, the distance travelled within, and the number of entries into each zone, were calculated. In addition, the total distance moved during the 10 min session was analyzed and the number of fecal boli dropped was counted at the end of each trial. <br>
#                           The application works for specific phenotypic tests and phenotypic measures. <br>
#                           Available phenotypic measures:</p>
#                           <h5 id='total-path-moved'>Total path moved</h5>
#                           <p>This is the total length of the path. It is sometimes termed “distance traveled” or “ambulatory distance”.</p>
#                           <h5 id='center-time'>Center time</h5>
#                           <p>The time spent in the center of the arena.</p>
#                                       <a href\"\" target=\"\"></a>
