
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
  skin = "blue",
  dashboardHeader(title = "Accordion Dashboard"),
  dashboardSidebar(
    # change dashboard sidebar appearance
    shinyjs::inlineCSS(list(
      ".sidebar-menu>li" = "font-size: 18px",
      ".sidebar-menu>li>a>.fa" = "width: 30px"
    )),
    sidebarMenu(
      menuItem("Import", tabName = "import", icon = icon("file-o")),
      menuItem("Pre-processing", tabName = "preProcessing", icon = icon("cogs")),
      menuItemOutput('selectmodeling'),
      menuItem("Evaluation", tabName = "evaluation", icon = icon("recycle")),
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
#             tabPanel(
#               "Date/Time parser", icon = icon("clock-o"),
#               # date time buttons
#               fluidRow(
#                 # allow x-flow for DT:dataTable
#                 shinyjs::inlineCSS(list(
#                   ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
#                 )),
#                 align = 'center',
#                 box(
#                   width = 6,
#                   title = "Predictor's Timestamp", 
#                   column(4,
#                       selectInput('preTimeCol', 
#                                   'Chose Column with timestamp:', 
#                                   choices = c('column'), 
#                                   multiple = F
#                       )
#                   ),
#                   column(8,
#                          uiOutput("predictor1Table")
#                          ),
#                   column(12,
#                          align = 'left',
#                          h5(strong('Timestamp Preview: ')),
#                          box(
#                            width = 12,
#                            align = 'center',
#                            h4(uiOutput('preTimestamp'))
#                          )
#                   ),
#                   column(6,
#                         h5('If integer convert to Timestamp'),
#                         actionButton('preTimeConvert','Convert')
#                   )
#                 ),
#                 box(
#                   width = 6,
#                   title = "Target's Timestamp", 
#                   column(4,
#                          selectInput('tarTimeCol', 
#                                      'Chose Column with timestamp:', 
#                                      choices = c('column'), 
#                                      multiple = F
#                          )
#                   ),
#                   column(8,
#                          uiOutput("target1Table")
#                         ),
#                   column(12,
#                          align = 'left',
#                          h5(strong('Timestamp Preview: ')),
#                          box(
#                            width = 12,
#                            align = 'center',
#                            h4(uiOutput('tarTimestamp'))
#                          )
#                   ),
#                   column(6,
#                          h5('If integer convert to Timestamp'),
#                          actionButton('preTimeConvert','Convert')
#                   )
#                 )
#               )
#             ),
            tabPanel(
              "Merge", icon = icon("compress"),
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
                    actionButton('merge', 'Merge', class="goButton", icon = icon("arrow-circle-right")),
                    bsModal("popMerge", "Merging Choices", "merge", size = "large", uiOutput("uiMerging"))
                       )
              )
            ),
            tabPanel(
              "Manage missing values", icon = icon("indent"),
              fluidRow(
                # interopolating/reapeting
                box(width = 12, 
                    conditionalPanel(
                      'output.targetIsNull',
                      column(6,
                             align = 'center',
                             h4('The selected Target is empty, please go to Merge tab and create the Merged file with a non-empty Target')
                              )
                    ),
                    conditionalPanel(
                      'output.targetWithNoNAvalues',
                      column(6,
                             align = 'center',
                             h4('The selected Target has no NA values. You can continue to Modeling or further Pre-processing')
                              )
                    ),
                    conditionalPanel(
                      "output.isTargetNumeric",
                      column(12,
                             column(4,
                                    selectizeInput(
                                      'theNApolicy', 
                                      "Select NA's Policy (numerical Target)",
                                      choices = c('Interpolation', 'Repeating'),
                                      multiple = F
                                    )
                             ),
                             conditionalPanel(
                               'input.theNApolicy == "Interpolation"',
                               column(4,
                                      radioButtons('interpolationMethod', 'Select an interpolation Method',
                                                   c(Linear ='linear',
                                                     Spline = 'spline',
                                                     Stineman = 'stine' ),                                                  
                                                   selected = 'linear',
                                                   inline = F)
                                      ),
                                column(12,
                                      # align = 'right',
                                      offset = 10,
                                      actionButton('interpolate',
                                                   'Interpolate')
                                 
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
                      "output.isTargetNonNumeric",
                      column(6, 
                             align = 'center',
                             h4("The selected target is Non-Numeric, as a result Repeat NA's Policy should be used"),
                             actionButton('repeating2', 'Repeat')
                      )
                    )
                  )
                )
            ),
            tabPanel(
              "Other Options", icon = icon("tasks"),
              fluidRow(
                # others like normalizing etc.
                shinyjs::inlineCSS(list(
                  "#shiny-tab-preprocessing .goButton" = "position: absolute; right:10px; bottom: 20px"
                )),
                box(
                  # status = "warning",
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
                  # height = "200px",
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
            "Plot", icon = icon("bar-chart"),
            fluidRow(
              #plots
              column(2,
                     radioButtons('plotType', 'Select Plot',
                                  c(Simple='simplePlot',
                                    Multiple='multiPlot',
                                    Correlation='corrPlot'),
                                  selected = 'simplePlot')
              ),
              column(3,
                     selectInput('plotX', 'X Variable', choices = c('Please select a dataset'), multiple = F)
              ),
              column(3,
                     selectInput('plotY', 'Y Variable(s)', choices = c('Please merge data for options'), multiple = T)
              ),
              conditionalPanel(
                'output.classPlotcheckNAs',
                column(3, h4(htmlOutput('textClassWithNA')))
              ),
              conditionalPanel(
                'output.classPlotcheck',
                column(3, selectInput('plotClass', textOutput('textClassSelector'), choices = c('Select Class'), multiple = T))
              ),
              column(1, align = 'right',
                     actionButton('preProsPlot', 'Plot', class="goButton", icon = icon("arrow-circle-right"))
              ),
              conditionalPanel(
                'output.simplePlotCheck',
                box(width = 12,
                    dygraphOutput("plot")
                )
              ),
              conditionalPanel(
                'output.multiPlotCheck',
                box(width = 12,
                    uiOutput("multi")
                )
              ),
              conditionalPanel(
                'output.corrPlotCheck',
                box(width = 12,
                    uiOutput("corr")
                )
              )
            )
          ),
          # allow x-flow for DT:dataTable
          shinyjs::inlineCSS(list(
            ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
          )),
          tabPanel(
            "Pre-Processed Data",icon = icon("table"), uiOutput("dataTable")
          ),
          tabPanel(
            "Summary",icon = icon("file-text-o"), verbatimTextOutput("dataSummary")
          ),
          tabPanel(
            tagList(shiny::icon("save"), "Save Datatable"), 
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
      ####                    Model                     ####
      ######################################################
      tabItem(
        tabName = 'model',
        fluidRow(
          box(width = 12,
              align = 'center',
              title = HTML('<i class="fa fa-info-circle" aria-hidden="true"></i> 
                            For more options choose your target variable'),
              h4(
              HTML('<b>Go to:</b>'),
              HTML('<p><i>"Pre processing"</i> <i class="fa fa-long-arrow-right" aria-hidden="true"></i>
                    <i>"Merge "</i> <i class="fa fa-long-arrow-right" aria-hidden="true"></i>
                    <i>"Choose your target variable"</i></p>')
              )
          )
        )
      ),
      tabItem(
        tabName = 'regression',
        fluidRow(
          box(width = 12,
              title = 'Regression',
              column(3,
                     column(12,
                            h5(strong('Predictors sampling rate is:')),
                            box(width = 8,
                                align = 'center',
                                h4(uiOutput('preSampleRateReg'))
                            )
                     )
              ),
              column(3,
                     # h5(strong('Give Target sampling rate:')),
                     numericInput('tarSampleRateReg',
                                  label = 'Give Target sampling rate:',
                                  min = 0, max = 1,
                                  value = 1, 
                                  step = 0.005)
              ),
              column(3,
                     column(12,
                            # h5(strong('Choose Number of samples:')),
                            numericInput('numOfSamplesReg',
                                         label = 'Choose Number of samples:',
                                         value = 10
                            )
                     ),
                     column(12,
                            numericInput("maxWindowReg", 
                                         label = "Choose Max window size:",
                                         min = 0, max = 20, value = 15, step = 1)
                     )
              ),
              conditionalPanel(
                'output.targetWithoutNAReg',
                column(3, 
                       radioButtons('regressionMethod', 'Choose Regression Method',
                                    c(Linear = 'linearReg',
                                      Lag  = 'lagReg'),                                                  
                                    selected = 'linearReg'),
                       column(12, align = 'right',
                              actionButton('goReg', 'Go', class="goButton", icon = icon("arrow-circle-right"))
                       )
                )
              ),
              conditionalPanel(
                'output.targetStillWithNAReg',
                column(3,
                       h4('The selected Target has NA values, go to Pre-processing Tab to Manage Missing values')
                  )
              ),
              conditionalPanel(
                'output.targetConstantReg',
                column(3,
                       h4('The selected Target is constant and cannot be predicted, please choose another target')
                )
              )
          ),
          tabBox(
            width = 12,
            # features data plot/review/summary
            tabPanel(
              "Plot Features",  icon = icon("bar-chart"),
              fluidRow(
                # running regression image
                conditionalPanel(
                  condition="$('html').hasClass('shiny-busy')",
                  column(12, align = 'center',
                         tags$img(src="loading_circle.gif")
                  )
                ),
                # features plot
                column(2,
                  radioButtons('featuresRegPlotType', 'Select Plot',
                               c(Simple='simplePlotReg',
                                 Multiple='multiPlotReg',
                                 Correlation='corrPlotReg'),
                               selected = 'simplePlotReg')
                ),
                column(3,
                       selectInput('plotRegX', 'X variable', choices = c('Please select a dataset'), multiple = F)
                ),
                column(3,
                       selectInput('plotRegY', 'Y variable(s)', choices = c('Please apply regression for options'), multiple = T)
                ),
                conditionalPanel(
                  'output.classPlotcheckReg',
                  column(3, selectInput('plotClassReg', textOutput('textClassSelectorReg'), choices = c('Select Class'), multiple = T))
                ),
                column(1, align = 'right',
                       actionButton('featuresRegPlot', 'Plot', class="goButton", icon = icon("arrow-circle-right"))
                ),
                conditionalPanel(
                  'output.simplePlotCheckReg',
                  box(width = 12,
                      dygraphOutput("plotReg")
                  )
                ),
                conditionalPanel(
                  'output.multiPlotCheckReg',
                  box(width = 12,
                      uiOutput("multiReg")
                  )
                ),
                conditionalPanel(
                  'output.corrPlotCheckReg',
                  box(width = 12,
                      uiOutput("corrReg")
                  )
                )
              )
            ),
            # allow x-flow for DT:dataTable
            shinyjs::inlineCSS(list(
              ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
            )),
            tabPanel(
              "Features Data",icon = icon("table"), 
              fluidRow(
                # running regression image
                conditionalPanel(
                  condition="$('html').hasClass('shiny-busy')",
                  column(12, align = 'center',
                         tags$img(src="loading_circle.gif")
                  )
                )
              ),
              # allow x-flow for DT:dataTable
              shinyjs::inlineCSS(list(
                ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
              )),
              uiOutput("featuresRegDataTable") 
            ),
            tabPanel(
              "Features Summary",icon = icon("file-text-o"),
              # running regression image
              conditionalPanel(
                condition="$('html').hasClass('shiny-busy')",
                column(12, align = 'center',
                       tags$img(src="loading_circle.gif")
                )
              ),
              verbatimTextOutput("featuresRegDataSummary")
            ),
            tabPanel(
              tagList(shiny::icon("save"), "Save Features Data"),
              fluidRow(
                div(
                  style="text-align:center; padding:30px",
                  downloadButton("featuresRegDataset",
                                 label = "Download features data table (.CSV)")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'classification',
        fluidRow(
          box(width = 12,
              title = 'Classification',
              column(3,
                     h5(strong('Predictors sampling rate is:')),
                     box(width = 5,
                         align = 'center',
                         h4(uiOutput('preSampleRateClass'))
                     )
              ),
              column(3,
                     # h5(strong('Give Target sampling rate:')),
                     numericInput('tarSampleRateClass',
                                  label = 'Give Target sampling rate:',
                                  min = 0, max = 1,
                                  value = 1, 
                                  step = 0.005)
              ),
              column(3,
                     column(12,
                            # h5(strong('Choose Number of samples:')),
                            numericInput('numOfSamplesClass',
                                         label = 'Choose Number of samples:',
                                         value = 10
                            )
                     ),
                     column(12,
                            numericInput("maxWindowClass", 
                                         label = "Choose Max window size:",
                                         min = 0, max = 20, value = 15, step = 1)
                     )
              ),
              conditionalPanel(
                'output.targetWithoutNAClass',
                column(12, align = 'right',
                       actionButton('goClass', 'Go', class="goButton", icon = icon("arrow-circle-right"))
                )
              ),
              conditionalPanel(
                'output.targetStillWithNAClass',
                column(3, align = 'center',
                       h4('The selected Target has NA values. Please go to: Pre-processing Tab to Manage Missing values')
                )
              ),
              conditionalPanel(
                'output.targetConstantClass',
                column(3,
                       h4('The selected Target is constant and cannot be predicted, please choose another target')
                )
              )
          ),
          tabBox(
            width = 12,
            # features data plot/review/summary
            tabPanel(
              "Plot Features",  icon = icon("bar-chart"),
              fluidRow(
                # running Classification image
                conditionalPanel(
                  condition="$('html').hasClass('shiny-busy')",
                  column(12, align = 'center',
                         tags$img(src="loading_circle.gif")
                  )
                ),
                # features plot
                column(2,
                       radioButtons('featuresClassPlotType', 'Select Plot',
                                    c(Simple='simplePlotClass',
                                      Multiple='multiPlotClass',
                                      Correlation='corrPlotClass'),
                                    selected = 'simplePlotClass')
                ),
                column(3,
                       selectInput('plotClassX', 'X variable', choices = c('Please select a dataset'), multiple = F)
                ),
                column(3,
                       selectInput('plotClassY', 'Y variable(s)', choices = c('Please apply Classification for options'), multiple = T)
                ),
                conditionalPanel(
                  'output.classPlotcheckClass',
                  column(3, selectInput('plotClassClass', textOutput('textClassSelectorClass'), choices = c('Select Class'), multiple = T))
                ),
                column(1, align = 'right',
                       actionButton('featuresClassPlot', 'Plot', class="goButton", icon = icon("arrow-circle-right"))
                ),
                conditionalPanel(
                  'output.simplePlotCheckClass',
                  box(width = 12,
                      dygraphOutput("plotClass")
                  )
                ),
                conditionalPanel(
                  'output.multiPlotCheckClass',
                  box(width = 12,
                      uiOutput("multiClass")
                  )
                ),
                conditionalPanel(
                  'output.corrPlotCheckClass',
                  box(width = 12,
                      uiOutput("corrClass")
                  )
                )
              )
            ),
            tabPanel(
              "Features Data",icon = icon("table"), 
              fluidRow(
                # running classification image
                conditionalPanel(
                  condition="$('html').hasClass('shiny-busy')",
                  column(12, align = 'center',
                         tags$img(src="loading_circle.gif")
                  )
                )
              ),
              # allow x-flow for DT:dataTable
              shinyjs::inlineCSS(list(
                ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
              )),
              uiOutput("featuresClassDataTable") 
            ),
            tabPanel(
              "Features Summary",icon = icon("file-text-o"), 
              fluidRow(
                # running classification image
                conditionalPanel(
                  condition="$('html').hasClass('shiny-busy')",
                  column(12, align = 'center',
                         tags$img(src="loading_circle.gif")
                  )
                ),
                verbatimTextOutput("featuresClassDataSummary") 
              )
            ),
            tabPanel(
              tagList(shiny::icon("save"), "Save Features Data"),
              div(
                style="text-align:center; padding:30px",
                downloadButton("featuresClassDataset",
                               label = "Download features data table (.CSV)")
              )
            )
          )
        )
      ),
      #</model>      

      ######################################################
      ####                 Evaluation                   ####
      ######################################################
      tabItem(
        tabName = 'evaluation',
        fluidRow(
          conditionalPanel(
            'output.isFeaturesEmpty',
            tabBox(
              id = "evalEmpty",
              width = 12,
              tabPanel(
                "Create Model", icon = icon("info"),
                fluidRow(
                  align = 'center',
                  column(6, 
                    # align = 'center',
                    h4(
                      HTML('<b>Go to:</b>'),
                      HTML('<p><i>"Pre processing"</i> <i class="fa fa-long-arrow-right" aria-hidden="true"></i>
                           <i>"Merge "</i> <i class="fa fa-long-arrow-right" aria-hidden="true"></i>
                           <i>"Choose your target variable"</i></p>'),
                      HTML('<b>And then:</b>'),
                      HTML('<p><i>"Model (Regression or Classification) "</i></p>')
                      )
                    )
                )
              ),
              tabPanel(
                "Import Model", icon = icon("upload"),
                fluidRow(
                  column(12,
                    align = 'center',
                    HTML('IMPORT MODEL'),
                    HTML('<p>IMPORT DATA</p>')
                  )
                )
              )
            )
          ),
          conditionalPanel(
            'output.featuresNonEmpty',
            tabBox(
              id = "evalTabs",
              width = 12,
              tabPanel(
                "Model Summary", icon = icon("file-text-o"),
                fluidRow(
                  # model summary  
                  box(
                    width = 12,
                    verbatimTextOutput("featuresStatisticsSummary") 
                  )
                )
              ),
              tabPanel(
                "Visualize Model", icon = icon("sitemap"), #("file-image-o"),
                fluidRow(
                  # visualization
                  # regression
                  conditionalPanel(
                    "output.isRegression",
                    box(width = 12,
                        title = 'Plot: Real target vs Predicted target',
                        dygraphOutput("targetTargetPlot")
                    ),
                    box(
                      width = 12,
                      verbatimTextOutput("modelSummary") 
                    )
                  ),
                  # classification
                  conditionalPanel(
                    "output.isClassification",
                    box(
                      width = 12,
                      title = "Decision Tree",
                      plotOutput("decisionTree")
                    )
                  )
                )
              ),
              tabPanel(
                tagList(shiny::icon("download"), "Export Model"),
                div(
                  style="text-align:center; padding:30px",
                  downloadButton("exportModel",
                                 label = "Export Model (.rda)")
                ),
                div(
                  style="text-align:center; padding:30px",
                  downloadButton("exportModelData",
                                 label = "Export Model Dataset (.csv)")
                )
              ),
              tabPanel(
                "Import Model", icon = icon("upload"),
                fluidRow(
                  # import
                  box(
                    width = 12,
                    title = "Import Model",
                    fluidRow(
                      column(
                        12,
                        uiOutput('ImportModel')
                        # fileInput('file1', 'Choose CSV File',
                        #           accept=c('text/csv',
                        #                    'text/comma-separated-values,text/plain',
                        #                    '.csv'))
                        
                      )
                    )
                  ),
                  box(
                    width = 12,
                    title = "Import Data",
                    fluidRow(
                      box(
                        width = 4,
                        radioButtons('sep', 'Separator',
                                     c(Comma=',',
                                       Semicolon=';',
                                       Tab='\t'),
                                     ',')
                      ),
                      box(
                        width = 4,
                        radioButtons('quote', 'Quote',
                                     c(None='',
                                       'Double Quote'='"',
                                       'Single Quote'="'"),
                                     '"')
                      ),
                      box(
                        width = 4,
                        checkboxInput('header', 'Header', TRUE)
                        
                      )
                    ),
                    fluidRow(
                      box(
                        width = 12,
                        uiOutput('ImportModelDataset')
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
                        tabPanel("Model Data", uiOutput("modelDataTable")),
                        tabPanel("Summary", verbatimTextOutput("modelDataSummary"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
        
      ),
      #</evaluation> 
      ######################################################
      ######################   About  ######################
      ######################################################
      
      tabItem(
        tabName = 'about',
        fluidRow(  
          box(
            title = "About",
            width = 12,
            HTML('<p>Acordion Dashboard aims to provide toolkits to explore datasets in a visualized way, especially for time-series datasets. It includes dataset uploading, pre-processing (merge, manage missing values, plot etc.), modeling (regression, classification), evaluation (model summary, visualization, export-import model)  which offer handy access of different methods and parameters applied to your data.</p>'),
            HTML('<p>Author: Ricardo Cachucho <a href="mailto:r.cachucho@liacs.leidenuniv.nl">r.cachucho@liacs.leidenuniv.nl</a>, Stylianos Paraschiakos <a href="mailto: s.paraschiakos@umail.leidenuniv.nl"> s.paraschiakos@umail.leidenuniv.nl</a></p>')
          )
        )
      )
      #</about>
      
    )
  )
)