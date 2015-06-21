library(shiny)
library(dplyr)
library(shinyIncubator)
library(shinyAce)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  navbarPage(title = HTML("Genotype-Lab Replicability Analyzer"),
             tabPanel("App",

#                       actionButton("Example4b", "Example 4 Stats.",class="ResetBtn",icon = icon("refresh")),
#                       actionButton("Example4a", "Example 4 param.",class="ResetBtn",icon = icon("refresh")),
#                       actionButton("Example3b", "Example 3 Stats.",class="ResetBtn",icon = icon("refresh")),
#                       actionButton("Example3a", "Example 3 param.",class="ResetBtn",icon = icon("refresh")),
#                       actionButton("Example2b", "Example 2 Stats.",class="ResetBtn",icon = icon("refresh")),
#                       actionButton("Example2a", "Example 2 param.",class="ResetBtn",icon = icon("refresh")),
#                       actionButton("Example1b", "Example 1 Stats.",class="ResetBtn",icon = icon("refresh")),
#                       actionButton("Example1a", "Example 1 param.",class="ResetBtn",icon = icon("refresh")),
#                       HTML("<div style='clear: both;'></div>"),
                     fluidRow(
                        column(8, offset = 2,
                               
                               h2("Genotype-Lab Replicability Analyzer",style = 'font-family: "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif;font-weight: 300;'),
                               h4("A tool for replicability assessment of mouse phenotyping results across laboratories",style = 'font-family: "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif;font-weight: 300;'),
                               
                               hr(),
                               
                               HTML("<h5><div class='step'>Step 1:</div> Fill in lab name and the genotype groups participated in the experiment ; Choose the experiment design for the multiple comparisons correction; Choose a unique experiment identifier or use default:</h5>"),
                               
                               wellPanel(
                                 #selectizeInput("lab_name",'Lab name:', choices = lab_names_vec, multiple = F, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list or add one',onInitialize = I('function() { this.setValue(""); }'))),
                                 textInput("lab_name", "Lab name:",value = "your_lab_name"),
                                 radioButtons("expr_design",'Experiment design:', choices = c("Pairwise Comparisons","Control vs. Cases"),inline = T),
                                 conditionalPanel(condition = "input.expr_design == 'Pairwise Comparisons'",
                                        selectizeInput("genotypes_tested_pairwise",'Genotypes tested (pairwise comparisons):', choices = genotypes_vec, multiple = TRUE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
                                        ),
                                 conditionalPanel(condition = "input.expr_design == 'Control vs. Cases'",
                                        selectizeInput("genotypes_tested_control",'Genotype tested (control group):', choices = genotypes_vec, multiple = FALSE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list one',onInitialize = I('function() { this.setValue(""); }'))),
                                        selectizeInput("genotypes_tested_cases",'Genotypes tested (cases groups):', choices = genotypes_vec, multiple = TRUE, options = list(create = TRUE, maxOptions = 5, placeholder = 'Select from the list two or more',onInitialize = I('function() { this.setValue(""); }')))
                                 ),
                                 radioButtons("proc_gender",'Gender:', choices = c("Males","Females","Males & Females"),inline = T),
                                 textInput("experiment_identifier", "Experiment identifier:",value = ""),
                                 actionButton("reset_experiment", " Reset",class="ResetBtn",icon = icon("refresh")), # add reset by package
                                 HTML("<div style='clear: both;'></div>")
                               ),
                               
                               HTML("<h5><div class='step'>Step 2:</div> Select procedure (test); Verify procedure's details in the link to Standard Operating Procedure; Fill-in any changed meta-data parameters:</h5>"),
                               
                               wellPanel(
                                 selectizeInput("proc_name",'Procedure Name:',
                                                choices = proc_name_list, #setNames(nm = as.character(unique(dat$Test)[c(7,4,1,2,3,5,6,8:(8+33))]),as.list(as.character(unique(dat$Test)[c(7,4,1,2,3,5,6,8:(8+33))])))),
                                                options = list(onInitialize = I('function() { this.setValue(""); }'))),
                                 htmlOutput("proc_SOP_link"),
                                 p("Following are the default meta-data for the procedure. Correct or add values as used in your experiment:"),
                                 aceEditor("meta_data_editor",".",theme="ambiance",mode = "r",height = "200px"),
                                 numericInput("proc_age", "Age at testing [weeks]:", ""),
                                 numericInput("proc_duration", "Procedure duration [min]:", ""),
                                 HTML(
                                   "<div class='form-group shiny-input-container'>
                                        <label for='proc_notes'>Procedure notes:</label>
                                        <textarea id='proc_notes' class='form-control' rows='2' cols='50' value=''></textarea>
                                   </div> "
                                 ),
                                 actionButton("reset_proc", " Reset",class="ResetBtn",icon = icon("refresh")),
                                 HTML("<div style='clear: both;'></div>")
                                 #actionButton(inputId = "submit_editor",label = "Submit"),
                               ),
                               
                               HTML("<h5><div class='step'>Step 3:</div> Select phenotypic measure\\s (note the meassuring unit and transformation):</h5>"),
                               
                               wellPanel(
                                 selectizeInput("measure_name",'Measure',
                                                choices = measure_name_list ,#  setNames(nm = paste0(dat$Meassure[1],"[",dat$unit[1],",",dat$trans[1],"]"),object = as.list(paste0(dat$Meassure[1],"[",dat$unit[1],",",dat$trans[1],"]"))),width = "auto"),
                                                options = list(onInitialize = I('function() { this.setValue(""); }'))),
                                 htmlOutput("selected_measure_details"),     
                                 actionButton("reset_measure", " Reset",class="ResetBtn",icon = icon("refresh")),
                                 HTML("<div style='clear: both;'></div>")
                               ),
                               
                               HTML("<h5><div class='step'>Step 4:</div> Fill in the resulted statistics from your experiment:</h5>"),
                               
                               wellPanel(
                                 conditionalPanel(
                                   condition = 'input.genotypes_tested != NULL',
                                   htmlOutput("stats_tbl") # stats table for each meassure, add note about multiplicity
                                 ),
                                 actionButton("reset_stats_tbl", " Reset",class="ResetBtn",icon = icon("refresh")),
                                 HTML("<div style='clear: both;'></div>")
                               ),
                               
                               HTML("<h5><div class='step'>Get Results:</div> </h5>"),
                               
                               wellPanel(
                                 
                                 checkboxInput("checkbox_agrees_share","Contribute the project by sending the experiment settings and results to our database."), # improve
                                 actionButton("submit_data", "Submit", icon = icon("cog")),
                                 
                                 h5("Summary table:"),
                                 hr(),
                                 tableOutput("out_tbl"),
                                 hr(),
                                 downloadButton("dl_results_button","Download results and inputs")
                                 
                               ),
                               
                               hr(),
                               h5("Diagram:")
                               # add plot here
                        ) # end column
                      ) # end fluid row
             ),
             
             tabPanel("Introduction",
                      fluidRow(
                        column(6,offset = 3,
                               h3("Introduction"),
                               p("This application takes mouse phenotyping results measured in your laboratory, and estimates how likely they are to be replicated in other laboratories, using the Random Lab Model with the GxL-adjusting method, as detailed in our manuscript &ldquo;Assessing the replicability of single-laboratory discoveries in phenotyping experiments&rdquo; (submitted)."),
                               p("The application takes as input single-lab phenotyping results from animals belonging to several genotype groups (for example: knockouts vs wildtype controls). The method outputs the p-value of the difference between the groups, using the standard t-test or ANOVA, and the &ldquo;r-value&rdquo; adjusted using previously estimated Genotype-by-Laboratory interaction (GxL). This replicability across laboratory p-value is the more reliable p-value for assessing the replicability of any single-lab genotype effect."),
                               p("Knowing the relevant GxL is the difficult part. The application is therefore limited to phenotypic tests, phenotyping measures and testing conditions for which we can currently us by contributing your phenotyping results and specifying the housing and testing conditions in your laboratory. These will be used to calculate a better estimate of GxL variability, based on more genotypes, more laboratories and more conditions, that will serve you and other researchers over the world in ensuring the replicability of their future phenotyping results. You also have the option not to contribute your results, for example if you do not trust them, or if you are only testing the application with hypothetical values.")
                        ))),
             tabPanel("Instructions",
                      fluidRow(
                        column(6,offset = 3,
                               HTML("
                                    <h3 id='instructions'>Instructions</h3>
                                    <h4 id='a-example'>A. Example</h4>
                                    <p>You can start by running a built-in example: </p>
                                    <ol>
                                    <li>Click “Fill in Example Data” (on the upper right) to fill test properties (sections 1-3). </li>
                                    <li>Click “Fill in Results” to fill experiment group data (section 4). </li>
                                    <li>Click “Calculate”.  </li>
                                    </ol>
                                    <p>Under “Results” you will get the difference between the genotype means, the p-value of the genotype effect using standard t-test (or ANOVA if there’s more than 2 genotypes), and the r-value – the adjusted p-value for the Genotype x Lab (GxL) interaction. <br>
                                    The example currently uses existing data for the Open Field Test (OFT), the phenotypic measure of “total path moved”, and the genotypes C57BL6NCrl and DBA/2NCrl.</p>
                                    <h4 id='b-inserting-your-data'>B. Inserting your data</h4>
                                    <p>Whenever you want to clear any of the sections, click the <code>reset</code> button next to this section. You can insert your own data. <br>
                                    In <strong>section 1</strong> insert a Name or Identifier ,the Lab Address and the genotypes. Choose at least two genotypes for the comparison. More that two genotypes can be entered and all pairwise comparisons will be calculated.  <br>
                                    In <strong>section 2</strong> choose first in “Test Type” the phenotypic test you used to measure your results. The application currently has a selection of 6 phenotypic tests. The application cannot be used for a test missing from the list. <br>
                                    Once the test is chosen, the windows for the test conditions become available below it. Insert the values used in your study. <br>
                                    In <strong>section 3</strong> choose the phenotypic measure. Every Test Type has a list of at least one available measures. The application cannot be used for a measure missing from the list. <br>
                                    In <strong>section 4</strong> enter the test results, group mean, group standard deviation and group size for each genotype.</p>
                                    <h4 id='open-field-test'>Open field test</h4>
                                    <p>The Open Field Test (OFT) is probably the most frequently used of all behavioral tests in pharmacology and neuroscience. However, there is a wide variety of OFT procedures using different arena shapes and sizes, tracking technologies, tracking systems, session lengths and many specific parameters which might affect the results. Our available data currently represent Richter et al. 2011, which describe its methods as follows: <br>
                                    The apparatus consisted of an open box, 40 cm x 40 cm minimum size, virtually divided into various zones (corners, 5 cm wall zone, centre). Mice were placed into the centre of the empty open field arena and video tracked for 10 min. The time spent in, the distance travelled within, and the number of entries into each zone, were calculated. In addition, the total distance moved during the 10 min session was analyzed and the number of fecal boli dropped was counted at the end of each trial. <br>
                                    The application works for specific phenotypic tests and phenotypic measures. <br>
                                    Available phenotypic measures:</p>
                                    <h5 id='total-path-moved'>Total path moved</h5>
                                    <p>This is the total length of the path. It is sometimes termed “distance traveled” or “ambulatory distance”.</p>
                                    <h5 id='center-time'>Center time</h5>
                                    <p>The time spent in the center of the arena.</p>
                                    <hr>
                                    <h4 id='references'>References</h4>
                                    <p>S. H. Richter, J. P. Garner, B. Zipser, L. Lewejohann, N. Sachser, C. Touma, B. Schindler, S. Chourbaji, C. Brandwein, P. Gass, N. van Stipdonk, J. van der Harst, B. Spruijt, V. V?ikar, D. P. Wolfer, H. W?rbel, <strong>Effect of population heterogenization on the reproducibility of mouse behavior: a multi-laboratory study.</strong> <em>PLoS One</em> 6(1):e16461 (2011). doi: 10.1371/journal.pone.0016461.</p>")
                               )
                        )
                      )
             )
  )
)
