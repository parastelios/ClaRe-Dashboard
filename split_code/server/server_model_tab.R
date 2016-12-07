# checking if target is null, numeric or not numeric to use it for the side menu (model, regression, classification)
output$selectmodeling <- renderMenu({
  target = tail(colnames(v$data),1)
  
  if(is.numeric(v$data[1,target])){
    menuItem("Regression", tabName = 'regression', icon = icon("th"))  
  }
  else if(! is.numeric(v$data[1,target]) && ! is.null(v$data[1,target])){
    menuItem("Classification", tabName = 'classification', icon = icon("th"))  
  }
  else{
    menuItem("Model", tabName = 'model', icon = icon("th"))  
  }
})

# event of merging creates options for the models (regression/classification)
observeEvent(input$confirmMerging,{
  # print(input$maxWindowClass)
  # Calculate sampling rate of predictors
  v$AIData <- analyseIndependentData(v$data_pre)
  v$AIData$Stable.Sampling.Rate <- "yes"
  
  # predictor rate
  v$preRate <- v$AIData$Sampling.Rate
  
  # target rate
  tarLocalSR <- 1/diff(v$data_tar[,input$targetField]/1000)
  v$tarRate <- median(tarLocalSR)
  
  # update the regression options after merge
  updateNumericInput(session, "tarSampleRateReg",
                     value = v$tarRate, 
                     min = 0, max = v$preRate, step = 0.005)  
  v$maxWinReg <- v$preRate*(input$numOfSamplesReg)
  updateNumericInput(session, "maxWindowReg",
                    value = v$maxWinReg,
                    min = 0, max = 10*(v$maxWinReg), step = 1)
  v$PParameterReg <- parameterFinder(v$AIData$Sampling.Rate, input$tarSampleRateReg, input$maxWindowReg)
  
  # update the classification options after merge
  updateNumericInput(session, "tarSampleRateClass", 
                     # label = 'Give Target sampling rate:',
                     value = v$tarRate, 
                     min = 0, max = v$preRate, step = 0.005)
  v$maxWinClass <- v$preRate*(input$numOfSamplesClass)
  updateNumericInput(session, "maxWindowClass",
                    value = v$maxWinClass,
                    min = 0, max = 10*v$maxWinClass, step = 1)
  
  # calculate the parameters that go into Accordion
  v$PParameterClass <- parameterFinder(v$AIData$Sampling.Rate, input$tarSampleRateClass, input$maxWindowClass)
})

##################
# Regression tab #

# checking if target has NAs
output$targetStillWithNAReg <- reactive({
  targetStillWithNAReg = (anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetStillWithNAReg)
})
outputOptions(output, 'targetStillWithNAReg', suspendWhenHidden = FALSE)

output$targetWithoutNAReg <- reactive({
  targetWithoutNAReg = (!anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetWithoutNAReg)
})
outputOptions(output, 'targetWithoutNAReg', suspendWhenHidden = FALSE)

# predictor sample rate
output$preSampleRateReg <- renderText({
  if (is.null(v$preRate)){
    return()
  }
  else{
    paste(v$preRate, 'Hz')
  }
})

# target sample rate is updated in merge_tab, while pressing merge button

# Adjusting max window size
observeEvent(input$numOfSamplesReg,{
  # Check preRate
  if(is.null(v$preRate)) {
    v$preRate <- 1
  }
  # print(v$preRate)
  v$maxWinReg <- v$preRate*(input$numOfSamplesReg)
  updateNumericInput(session, "maxWindowReg",
                     value = v$maxWinReg,
                     min = 0, max = 10*v$maxWinReg, step = 1)
})

# Running Accordion:
observeEvent(input$goReg,{
  
  # Update sampling rate of predictors after pre-processing
  v$AIData <- analyseIndependentData(v$data[,-ncol(v$data)])
  v$AIData$Stable.Sampling.Rate <- "yes"
  
  # Update Accordion parameters according to the users preferences
  v$PParameterReg <- parameterFinder(v$AIData$Sampling.Rate, input$tarSampleRateReg, input$maxWindowReg)
  v$PParameterReg$nOperations <- input$numOfSamplesReg
  v$PParameterReg$Size <- round(seq(from=v$PParameterReg$Jump, to=input$maxWindowReg, length.out=v$PParameterReg$nOperations), digits = 0)
  
  # Downsample the target
  aTarIndex <- seq(from = v$PParameterReg$Jump, to = v$AIData$Variables.nrow, by = v$PParameterReg$Jump)
  aTarIndex[length(aTarIndex)]
  
  # Run accordion for lag regression
  if(input$regressionMethod == 'linearReg'){
    print('linear')
    v$features <- embedded.Cor.FS(v$data[aTarIndex,ncol(v$data)], v$data[1:aTarIndex[length(aTarIndex)],-ncol(v$data)], v$AIData, v$PParameterReg)
  }
  else{
    print('lag')
    v$features <- embedded.CCF.FS(v$data[aTarIndex,ncol(v$data)], v$data[1:aTarIndex[length(aTarIndex)],-ncol(v$data)], v$AIData, v$PParameterReg) 
  }
  # TODO: if condition to allow to change the regression to linear regression
  # Run accordion for linear regression
  #
  print(summary(v$features))
  
  # build regression model
  linearModel <- lm(DData ~ ., data=v$features)
  print(summary.lm(linearModel))
  # TODO: plot predicted vs real target with dygraphs
  print(summary(predict(linearModel)))
  
  renderFeaturesRegDataTable(v$features)
})

# render feature data Reg function
renderFeaturesRegDataTable <- function(data) {
  output$featuresRegDataTable <- renderUI({
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
      output$dataTable0 <- DT::renderDataTable({
        DT::datatable(data, options = list(pageLength = 20))
      })
      DT::dataTableOutput('dataTable0')
    }
  })
  
  output$featuresRegDataSummary <- renderPrint({
    if (is.null(data))
      return()
    summary(data)
  })
}

######################
# Classification tab #

# checking if target has NAs
output$targetStillWithNAClass <- reactive({
  targetStillWithNAClass = (anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetStillWithNAClass)
})
outputOptions(output, 'targetStillWithNAClass', suspendWhenHidden = FALSE)

output$targetWithoutNAClass <- reactive({
  targetWithoutNAClass = (!anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
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
      output$dataTable0 <- DT::renderDataTable({
        DT::datatable(data, options = list(pageLength = 20))
      })
      DT::dataTableOutput('dataTable0')
    }
  })
  
  output$featuresClassDataSummary <- renderPrint({
    if (is.null(data))
      return()
    summary(data)
  })
}
