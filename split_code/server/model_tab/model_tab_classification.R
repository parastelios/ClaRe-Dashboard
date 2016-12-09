######################
# Classification tab #

# checking if target has NAs
output$targetStillWithNAClass <- reactive({
  targetStillWithNAClass = (anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetStillWithNAClass)
})
outputOptions(output, 'targetStillWithNAClass', suspendWhenHidden = FALSE)

# checking if target is constant
output$targetConstantClass <- reactive({
  targetConstantClass = (length(unique(v$data[,ncol(v$data)]))==1 && !anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetConstantClass)
})
outputOptions(output, 'targetConstantClass', suspendWhenHidden = FALSE)

# checking if target can be predicted
output$targetWithoutNAClass <- reactive({
  targetWithoutNAClass = (!anyNA(v$data[,ncol(v$data)]) && !length(unique(v$data[,ncol(v$data)]))==1 && !is.null(v$data[,ncol(v$data)]))
  return(targetWithoutNAClass)
})
outputOptions(output, 'targetWithoutNAClass', suspendWhenHidden = FALSE)

# predictor sample rate
output$preSampleRateClass <- renderText({
  if (is.null(v$preRate)){
    return()
  }
  else{
    paste(v$preRate, 'Hz')
  }
})

# target sample rate is updated in merge_tab, while pressing merge button

# Adjusting max window size
observeEvent(input$numOfSamplesClass,{
  # Check preRate
  if(is.null(v$preRate)) {
    v$preRate <- 1
  }
  v$maxWinClass <- v$preRate*(input$numOfSamplesClass)
  updateNumericInput(session, "maxWindowClass",
                     value = v$maxWinClass,
                     min = 0, max = 10*(v$maxWinClass), step = 1)
})

# Running Accordion:
observeEvent(input$goClass,{
  # Update sampling rate of predictors after pre-processing
  v$AIData <- analyseIndependentData(v$data[,-ncol(v$data)])
  v$AIData$Stable.Sampling.Rate <- "yes"
  # print(input$maxWindowClass)
  # Update Accordion parameters according to the users preferences
  v$PParameterClass <- parameterFinder(v$AIData$Sampling.Rate, input$tarSampleRateClass, input$maxWindowClass)
  v$PParameterClass$nOperations <- input$numOfSamplesClass
  v$PParameterClass$Size <- round(seq(from=v$PParameterClass$Jump, to=input$maxWindowClass, length.out=v$PParameterClass$nOperations), digits = 0)
  #print(v$PParameterClass)
  
  # Downsample the target
  aTarIndex <- seq(from = v$PParameterClass$Jump, to = v$AIData$Variables.nrow, by = v$PParameterClass$Jump)
  
  # Run accordion
  v$features <- embeddedGainRatioFS(v$data[aTarIndex,ncol(v$data)], v$data[1:aTarIndex[length(aTarIndex)],-ncol(v$data)], v$AIData, v$PParameterClass)
  print(summary(v$features))
  
  # Train decision tree and record accuracy
  treeModel <- J48(DData ~ ., data=v$features)
  print(summary(treeModel, numFolds = 10))
  # TODO: plot decision tree
  plot(treeModel,cex=0.5)
  
  renderFeaturesClassDataTable(v$features)
})

# render feature data Class function
renderFeaturesClassDataTable <- function(data) {
  output$featuresClassDataTable <- renderUI({
    # print(data)
    if (is.null(data)){
      fluidRow(box(
        width = 12,
        background ="red",
        tags$h4(icon('bullhorn'),"Features Data NULL!")
        #HTML("Please upload a dataset to start.")
      ))
    }
    else{
      output$featursDataClassTable0 <- DT::renderDataTable({
        DT::datatable(data, options = list(pageLength = 20))
      })
      DT::dataTableOutput('featursDataClassTable0')
    }
  })
  
  output$featuresClassDataSummary <- renderPrint({
    if (is.null(data))
      return()
    summary(data)
  })
}