
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(dygraphs)
library(shinyBS)

# read external js 
fileName <- "shinyjsSeg.js"
# readLines import file asynchronously and fails shinyjs
# jsCode <- paste(readLines("./shinyjsSeg.js"), collapse=" ")
jsCode <- readChar(fileName, file.info(fileName)$size)
fileName2 <- "shinyjsBi.js"
jsCode2 <- readChar(fileName2, file.info(fileName2)$size)

dashboardPage(
  dashboardHeader(title = "Analysis Dashboard"),
  dashboardSidebar(
    # change dashboard sidebar appearance
    shinyjs::inlineCSS(list(
      ".sidebar-menu>li" = "font-size: 18px",
      ".sidebar-menu>li>a>.fa" = "width: 30px"
    )),
    sidebarMenu(
      menuItem("Import", tabName = "import", icon = icon("file-o")),
      menuItem("Pre-processing", tabName = "preProcessing", icon = icon("cog")),
      
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    # import js
    tags$head(includeScript("html2canvas.js")),
    # tags$head(includeScript("download.js")),
    useShinyjs(),
    # extend js
    extendShinyjs(text = jsCode, functions = c("updateSeg", "prevSeg", "nextSeg", "showSeg", "saveSeg")),
    extendShinyjs(text = jsCode2, functions = c("updateBi", "prevBi", "nextBi", "showBi", "saveBi")),
    
    # import global css
    shinyjs::inlineCSS(list(
      ".shiny-progress .progress" = "height:10px !important;"
    )),
    
    
    tabItems(
      
      ######################################################
      ###################### import  ######################
      #####################################################
      tabItem(
        
        tabName = "import",
        fluidRow(
          
          box(
            width = 6,
            title = "Upload Predictors Data",
            fluidRow(
              box(
                width = 4,
                radioButtons('sepPre', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ',')
              ),
              box(
                width = 4,
                radioButtons('quotePre', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '"')
              ),
              box(
                width = 4,
                checkboxInput('headerPre', 'Header', TRUE)
                
              )
            ),
            fluidRow(
              box(
                width = 12,
                uiOutput('predictorImport')
                # fileInput('file1', 'Choose CSV File',
                #           accept=c('text/csv',
                #                    'text/comma-separated-values,text/plain',
                #                    '.csv'))
                
              ),
              
              # allow x-flow for DT:dataTable
              shinyjs::inlineCSS(list(
                ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
              )),
              tabBox(
                width = 12,
                tabPanel("Predictors Data", uiOutput("predictorTable")),
                tabPanel("Predictors Summary", verbatimTextOutput("predictorSummary"))
              )
            )
          ),
          box(
            width = 6,
            title = "Upload Target Data",
            fluidRow(
              box(
                width = 4,
                radioButtons('sepTar', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ',')
              ),
              box(
                width = 4,
                radioButtons('quoteTar', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '"')
              ),
              box(
                width = 4,
                checkboxInput('headerTar', 'Header', TRUE)
                
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                uiOutput('targetImport')
                # fileInput('file1', 'Choose CSV File',
                #           accept=c('text/csv',
                #                    'text/comma-separated-values,text/plain',
                #                    '.csv'))
              ) 
            ),
            
            # allow x-flow for DT:dataTable
            shinyjs::inlineCSS(list(
              ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
            )),
            tabBox(
              width = 12,
              tabPanel("Targets Data", uiOutput("targetTable")),
              tabPanel("Targets Summary", verbatimTextOutput("targetSummary"))
            )
          )
        )
      ),
      # </import>
      
      ######################################################
      ################# Pre-processing #####################
      ######################################################
      
      tabItem(
        tabName = 'preProcessing',
        fluidRow(
          # tabs for pre-processing
          tabBox( 
            id = "preProTabs",
            width = 12,
            tabPanel(
              "Date/Time parser",
              # date time buttons
              fluidRow(
                # allow x-flow for DT:dataTable
                shinyjs::inlineCSS(list(
                  ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
                )),
                align = 'center',
                box(
                  width = 6,
                  title = "Predictor's Timestamp", 
                  column(4,
                      selectInput('preTimeCol', 
                                  'Chose Column with timestamp:', 
                                  choices = c('column'), 
                                  multiple = F
                      )
                  ),
                  column(8,
                         uiOutput("predictor1Table")
                         ),
                  column(12,
                         align = 'left',
                         h5(strong('Timestamp Preview: ')),
                         box(
                           width = 12,
                           align = 'center',
                           h4(uiOutput('preTimestamp'))
                         )
                  ),
                  column(6,
                        h5('If integer convert to Timestamp'),
                        actionButton('preTimeConvert','Convert')
                  )
                ),
                box(
                  width = 6,
                  title = "Target's Timestamp", 
                  column(4,
                         selectInput('tarTimeCol', 
                                     'Chose Column with timestamp:', 
                                     choices = c('column'), 
                                     multiple = F
                         )
                  ),
                  column(8,
                         uiOutput("target1Table")
                        ),
                  column(12,
                         align = 'left',
                         h5(strong('Timestamp Preview: ')),
                         box(
                           width = 12,
                           align = 'center',
                           h4(uiOutput('tarTimestamp'))
                         )
                  ),
                  column(6,
                         h5('If integer convert to Timestamp'),
                         actionButton('preTimeConvert','Convert')
                  )
                )
              )
            ),
            tabPanel(
              "Merge",
              fluidRow(
                # merge data
                box(width = 4, 
                    title = '1. Select Merge Fields:',
                    column(12,
                           selectInput('targetField', 'Target', choices = c('Please select a field to merge upon'), multiple = F)
                           ),   
                    column(12,
                           selectInput('predictorField', 'Predictor', choices = c('Please select a field to merge upon'), multiple = F)
                    )
                ),
                box(width = 4,
                    title = '2. Choices:',
                    column(12,
                           selectInput('targetOption', 'Choose your Target variable:', choices = c('Please select Target'))
                           ),
                    column(12,
                           selectInput('excludingPre', 'Exclude Predictor variable(s)', choices = c('Select variable(s)'), multiple = T)
                          )
                ),
                box(width = 4, 
                    title = '3. Create merged Dataset:',
                    actionButton('merge', 'Go Merge', class="goButton", icon = icon("arrow-circle-right")),
                    bsModal("popMerge", "Merging Choices", "merge", size = "large", uiOutput("uiMerging"))
                       )
              )
            ),
            #     box(width = 6,
            #       column(10,
            #         selectInput('predictorField', 'Predictor Field', choices = c('Please select a field to merge upon'), multiple = F)
            #       ),
            #       column(10,
            #         selectInput('targetField', 'Target Field', choices = c('Please select a field to merge upon'), multiple = F)
            #       ),
            #       column(width = 2, onset = 2, actionButton("merge", "Merge"))
            #     ),
            #       
            #     )
            #   )
            # ),
            tabPanel(
              "Manage missing values",
              fluidRow(
                # interopolating/reapeting
                box(width = 12, 
                    column(6,
                           selectizeInput(
                             'colWithNAvalues', 'Select Target',
                             choices = c('No info to show'),
                             multiple = F,
                             options = list(placeholder = 'select variable(s)')
                             )
                    ),
                    conditionalPanel(
                      "output.mergecheck1",
                      column(6,
                             selectizeInput(
                               'theNApolicy', "The target is Numerical, Select NA's Policy",
                               choices = c('Repeating', 'Interpolation'),
                               multiple = F
                             ),
                             conditionalPanel(
                               'input.theNApolicy == "Interpolation"',
                               column(12, 
                                      box(
                                        title = 'Interpolation Methods',
                                        width = 8, 
                                        radioButtons('interpolationMethod', 'Method',
                                                     c(Linear ='linear',
                                                       Spline = 'spline',
                                                       Stineman = 'stine' ),                                                  
                                                     selected = 'linear',
                                                     inline = TRUE)
                                      ),
                                      column(4,
                                      actionButton('interpolate',
                                                   'Interpolate')
                                      )
                                )
                             ),
                             conditionalPanel(
                               'input.theNApolicy == "Repeating"',
                               column(12, offset = 10,
                                      actionButton('repeating1', 'Repeat')
                               )
                              )
                             )
                       ),
                    conditionalPanel(
                      "output.mergecheck2",
                      column(6, 
                             "Non-Numerical target, as a result Repeat NA's Policy should be used",
                             column(12, offset = 4,
                                    actionButton('repeating2', 'Repeat')
                             )
                      )
                    )
                  )
                )
            ),
            tabPanel(
              "Other Options",
              fluidRow(
                # others like normalizing etc.
                shinyjs::inlineCSS(list(
                  "#shiny-tab-preprocessing .goButton" = "position: absolute; right:10px; bottom: 20px"
                )),
                box(
                  width = 4,
                  height = "200px",
                  title = "Excludes",
                  selectInput('excludingVar', 'Exclude variable(s)', choices = c('Please Merge for options'), multiple = T),
                  column(12, align = 'right',
                         actionButton('goExcludingVar', 'Go', class="goButton", icon = icon("arrow-circle-right")),
                         bsModal("popExcludingVar", "Excludes", "goExcludingVar", size = "small", uiOutput("uiExcludingVar"))
                  )
                ),
                box(
                  width = 4,
                  height = "200px",
                  title = "Outlier Removal",
                  selectInput('outlierRemoval', 'Select a variable', choices = c('Please Merge for options'), multiple = F),
                  column(12, align ='right',
                         actionButton('goOutlierRemoval', 'Go', class="goButton", icon = icon("arrow-circle-right")),
                         bsModal("popOutlierRemoval", "Outlier Removal", "goOutlierRemoval", size = "large",uiOutput("uiOutlierRemoval"))
                         )
                ),
                box(
                  width = 4,
                  height = "200px",
                  title = "Data Normalization",
                  align = 'right',
                  actionButton('goNormalizing', 'Go', class="goButton", icon = icon("arrow-circle-right"))
                ),
                box(
                  width = 12,
                  title = "Conditions",
                  fluidRow(
                    column(3,selectInput('variableCon', 'If', choices = c('Please Merge for options'), multiple = F)),
                    conditionalPanel(
                      "output.condCheck1",
                      column(1,selectInput('equalCon1', '.', choices = c('==','>=','<=','>','<'), multiple = F)),
                      column(2,numericInput('numberCon', label='.', value=0)),
                      column(2,selectInput('actionCon1', 'Then', choices = c('Remove line','Replace with'), multiple = F)),
                      conditionalPanel("input.actionCon1 == 'Replace with'",
                                       column(2, 
                                              numericInput('replaceCon1', 
                                                           label='.', value=0)
                                       )
                      ),
                      column(2,
                             style="padding-top:30px; font-weight: bold",
                             textOutput('rowSelected1')
                      ),
                      column(12, align = 'right',
                             actionButton('goConditions1', 'Go', class="goButton", icon = icon("arrow-circle-right"))
                      )
                    ),
                    conditionalPanel(
                      "output.condCheck2",
                      column(2,selectInput('equalCon2', '.', choices = c('==','!='), multiple = F)),
                      column(2,textInput('textCon', label='.', value= '', placeholder = 'type input')),
                      # column(2,selectInput('textCon', '.', choices = c('Please merge for options'), multiple = F)),
                      column(2,selectInput('actionCon2', 'Then', choices = c('Remove line','Replace with'), multiple = F)),
                      conditionalPanel("input.actionCon2 == 'Replace with'",
                                       column(2,
                                              textInput('replaceCon2', label='.', value='', placeholder = 'type input')
                                       )
                      ),
                      column(2,
                             style="padding-top:30px; font-weight: bold",
                             textOutput('rowSelected2')
                      ),
                      column(12, align = 'right',
                             actionButton('goConditions2', 'Go', class="goButton", icon = icon("arrow-circle-right"))
                      )
                    ),
                    conditionalPanel(
                      "output.condCheck3",
                      column(4, align = 'center', h4("The chosen variable has NA values. Manage missing values for options "))
                    )
                  )
                )
              )
            )
          ),
          tabBox(
            # plot and data review
            width = 12,
            tabPanel(
              "Plotting",
              # fluidRow(
              #   #plots
              #   column(2,
              #     radioButtons('plotType', 'Select Plot',
              #                  c(Simple='simplePlot',
              #                    Multiple='multiPlot',
              #                    Correlation='corrPlot'),
              #                  selected = 'simplePlot')
              #   ),
              #   # column(4,
              #   #   selectInput('plotType', 'Select Plot', choices = c('Simple', 'Multiple', 'Correlation'), multiple = F)
              #   # )
              #   column(3,
              #          selectInput('plotX', 'X Varaible', choices = c('Please select a dataset'), multiple = F)
              #   ),
              #   column(3,
              #          selectInput('plotY', 'Y Varaible(s)', choices = c('Please merge data for options'), multiple = T)
              #   ),
              #   conditionalPanel(
              #     'output.classPlotcheck',
              #     column(3, selectInput('plotClass', textOutput('textClassSelector'), choices = c('Select Class'), multiple = T))
              #   ),
              #   column(3, align = 'right', offset = 8,
              #          actionButton('preProsPlot', 'Plot', class="goButton", icon = icon("arrow-circle-right"))
              #   ),
              #   tabBox(
              #         width = 12,
              #      dygraphOutput("plot")
              #      )
              # )
              fluidRow(
                #plots
                column(4,
                    selectInput('plotX', 'X Varaible', choices = c('Please select a dataset'), multiple = F)
                ),
                column(4,
                    selectInput('plotY', 'Y Varaible(s)', choices = c('Please merge data for options'), multiple = T)
                ),
                # conditionalPanel(
                #   'output.classPlotcheck',
                  column(4, selectInput('plotClass', textOutput('textClassSelector'), choices = c('Select Class'), multiple = T))
                # ),
                ,
                tabBox(
                  width = 12,
                  tabPanel("Plot", dygraphOutput("plot")),
                  tabPanel("Multi-plot", uiOutput("mulplot")),
                  tabPanel("Correlation", uiOutput("corplot"))
                )
              )
            ),
            # allow x-flow for DT:dataTable
            shinyjs::inlineCSS(list(
              ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
            )),
            tabPanel(
              "Pre-Processed Data", uiOutput("dataTable")
            ),
            tabPanel(
              "Summary", verbatimTextOutput("dataSummary")
            ),
            tabPanel(
              tagList(shiny::icon("download"), "Save"), 
              div(
                style="text-align:center; padding:30px",
                downloadButton("processedDataset", 
                label = "Download pre-processed data table (.CSV)")
              )
            )
          )
          
        )
      ),
      #</preprocessing>
      
      
      ######################################################
      ######################   About  ######################
      ######################################################
      
      tabItem(
        tabName = 'about',
        
        fluidRow(  
          box(
            title = "About",
            width = 12,
            HTML('<p>Analysis Dashboard aims to provide toolkits to explore datasets in a visualized way, especially for time-series datasets. It includes dataset uploading, plotting, pre-processing, segmentation and biclustering, which offer handy access of different methods and parameters applied to your data.</p>'),
            HTML('<p>Author: Ricardo Cachucho <a href="mailto:r.cachucho@liacs.leidenuniv.nl">r.cachucho@liacs.leidenuniv.nl</a>, Kaihua liu <a href="mailto:k.liu.5@umail.leidenuniv.nl">k.liu.5@umail.leidenuniv.nl</a></p>')
          )
        )
      )
      #</about>
      
    )
  )
  
)