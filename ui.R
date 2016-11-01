
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
              fluidRow(
                # date time buttons
              )
            ),
            tabPanel(
              "Merge",
              fluidRow(
                # merge data
                box(
                  width = 5,
                  selectInput('predictorField', 'Predictor Field', choices = c('Please select a field to merge upon'), multiple = F)
                ),box(
                  width = 5,
                  selectInput('targetField', 'Target Field', choices = c('Please select a field to merge upon'), multiple = F)
                ),
                column(width = 2, onset = 2, actionButton("merge", "Merge"))
              )
            ),
            tabPanel(
              "Manage missing values",
              fluidRow(
                # interopolating/reapeting
                box(width = 6, selectizeInput('colNAInterpolate', 'Interpolation',
                                              choices = c('No info to show'),
                                              multiple = T,
                                              options = list(placeholder = 'select column(s)')),
                    box(width = 8, radioButtons('interpolationMethod', 'Method',
                                                c(Linear ='linear',
                                                  Spline = 'spline',
                                                  Stineman = 'stine' ),                                                  
                                                selected = 'linear',
                                                inline = TRUE)
                    ),
                    actionButton('interpolate', 
                                 'Interpolate')
                ),
                box(
                  width = 6,
                  selectizeInput('colNARepeating', 
                                 'Repeat values', 
                                 choices = c('No info to show'), 
                                 multiple = T,
                                 options = list(placeholder = 'select column(s)')),
                  column(width = 2, offset = 10, actionButton('repeating', 'Repeat'))
                )
              )
            )
            # tabPanel(
            #   "Other Options",
            #   fluidRow(
            #     # others like normalizing etc.
            #     shinyjs::inlineCSS(list(
            #       "#shiny-tab-preprocessing .goButton" = "position: absolute; right:10px; bottom: 20px"
            #     )),
            #     box(
            #       width = 4,
            #       height = "250px",
            #       title = "Excludes",
            #       # selectInput('excludings', 'Value Range', choices = c('Please select a dataset'), multiple = T)
            #       selectInput('excludingPreVar', 'Exclude Predictor variable(s)', choices = c('Please select a Predictors'), multiple = T),
            #       selectInput('excludingTarVar', 'Exclude Target variable(s)', choices = c('Please select a Predictors'), multiple = T),
            #       actionButton('goExcludingVar', 'Go', class="goButton", icon = icon("arrow-circle-right")),
            #       bsModal("popExcludingVar", "Excludes", "goExcludingVar", size = "small", uiOutput("uiExcludingVar"))
            #     ),
            #     box(
            #       width = 4,
            #       height = "200px",
            #       title = "Outlier Removal",
            #       selectInput('outlierRemoval', 'Select a variable', choices = c('Please select a dataset'), multiple = F),
            #       actionButton('goOutlierRemoval', 'Go', class="goButton", icon = icon("arrow-circle-right")),
            #       bsModal("popOutlierRemoval", "Outlier Removal", "goOutlierRemoval", size = "large",uiOutput("uiOutlierRemoval"))
            #     ),
            #     box(
            #       width = 4,
            #       height = "200px",
            #       title = "Normalization",
            #       # checkboxInput('normalizing', 'Normalizing', FALSE),
            #       radioButtons('normalizing', 'Normalization',c('ON'=T,'OFF'=F),T)
            #       ,
            #       actionButton('goNormalizing', 'Go', class="goButton", icon = icon("arrow-circle-right"))
            #     ),
            #     box(
            #       width = 8,
            #       height = "210px",
            #       title = "Conditions",
            #       fluidRow(
            #         column(4,selectInput('variableCon', 'If', choices = c('.'), multiple = F)),
            #         column(4,selectInput('equalCon', '.', choices = c('==','>=','<=','>','<'), multiple = F)),
            #         column(4,numericInput('numberCon', label='.', value=0))
            #       ),
            #       fluidRow(
            #         column(4,selectInput('actionCon', 'Then', choices = c('Remove line','Replace with'), multiple = F)),
            #         
            #         conditionalPanel("input.actionCon == 'Replace with'",
            #                          column(4,numericInput('replaceCon', label='.', value=0))
            #         ),
            #         column(3, 
            #                style="padding-top:30px; font-weight: bold",
            #                textOutput('rowSelected'))
            #       ),
            #       
            #       actionButton('goConditions', 'Go', class="goButton", icon = icon("arrow-circle-right"))
            #     )
            #     
            #   )
            # )
          ),
          tabBox(
            # plot and data review
            width = 12,
            tabPanel(
              "Plotting",
              fluidRow(
                #plots
                box(width = 6,
                    selectInput('plotX', 'X Varaible', choices = c('Please select a dataset'), multiple = F)
                ),
                box(width = 6,
                    selectInput('plotY', 'Y Varaible(s)', choices = c('Please select a dataset'), multiple = T)
                ),
                tabBox(
                  width = 12,
                  tabPanel("Plot", dygraphOutput("plot"))
                  ,tabPanel("Multi-plot", uiOutput("mulplot"))
                  ,tabPanel("Correlation", uiOutput("corplot"))
                )
              )
            ),
            
            tabPanel(
              "Pre-Processed Data",
              fluidRow(
                width = 12, uiOutput("dataTable")
              )
            ),
            tabPanel(
              "Summary",
              fluidPage(
                width = 12, verbatimTextOutput("dataSummary")
              )
            ),
            tabPanel(tagList(shiny::icon("download"), "Save"), 
                     div(
                       style="text-align:center; padding:30px",
                       downloadButton("processedDataset", label = "Download pre-processed data table (.CSV)")
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