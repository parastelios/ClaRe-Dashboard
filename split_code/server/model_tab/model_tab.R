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
