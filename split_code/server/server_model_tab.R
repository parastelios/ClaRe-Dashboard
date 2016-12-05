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
  # Load Accordion code (Class/Regres)
  source('rQuickie.R')
  source('featureSelection.R')
  source('featureConstruction.R')
  
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
  maxWinReg <- v$preRate*(input$numOfSamplesReg)
  updateSliderInput(session, "maxWindowReg",
                    value = maxWinReg,
                    min = 0, max = 10*(maxWinReg), step = 5)
  v$PParameterReg <- parameterFinder(v$AIData$Sampling.Rate, input$tarSampleRateReg, input$maxWindowReg)
  
  # update the classification options after merge
  updateNumericInput(session, "tarSampleRateClass", 
                     # label = 'Give Target sampling rate:',
                     value = v$tarRate, 
                     min = 0, max = v$preRate, step = 0.005)
  maxWinClass <- v$preRate*(input$numOfSamplesClass)
  updateSliderInput(session, "maxWindowClass",
                    value = maxWinClass,
                    min = 0, max = 10*(maxWinClass), step = 5)
  
  # calculate the parameters that go into Accordion
  v$PParameterClass <- parameterFinder(v$AIData$Sampling.Rate, input$tarSampleRateClass, input$maxWindowClass)
})

##################
# Regression tab #

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
  maxWinReg <- v$preRate*(input$numOfSamplesReg)
  updateSliderInput(session, "maxWindowReg",
                    value = maxWinReg,
                    min = 0, max = 10*(maxWinReg), step = 5)
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
  features <- embedded.CCF.FS(v$data[aTarIndex,ncol(v$data)], v$data[1:aTarIndex[length(aTarIndex)],-ncol(v$data)], v$AIData, v$PParameterReg)
  # TODO: if condition to allow to change the regression to linear regression
  # Run accordion for linear regression
  #features <- embedded.Cor.FS(v$data[aTarIndex,ncol(v$data)], v$data[1:aTarIndex[length(aTarIndex)],-ncol(v$data)], v$AIData, v$PParameterReg)
  print(summary(features))
  
  # build regression model
  linearModel <- lm(DData ~ ., data=features)
  print(summary.lm(linearModel))
  # TODO: plot predicted vs real target with dygraphs
  print(summary(predict(linearModel)))
  
})

######################
# Classification tab #

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
  maxWinClass <- v$preRate*(input$numOfSamplesClass)
  updateSliderInput(session, "maxWindowClass",
                    value = maxWinClass,
                    min = 0, max = 10*(maxWinClass), step = 5)
  
})

# Running Accordion:
observeEvent(input$goClass,{
  # Update sampling rate of predictors after pre-processing
  v$AIData <- analyseIndependentData(v$data[,-ncol(v$data)])
  v$AIData$Stable.Sampling.Rate <- "yes"
  
  # Update Accordion parameters according to the users preferences
  v$PParameterClass <- parameterFinder(v$AIData$Sampling.Rate, input$tarSampleRateClass, input$maxWindowClass)
  v$PParameterClass$nOperations <- input$numOfSamplesClass
  v$PParameterClass$Size <- round(seq(from=v$PParameterClass$Jump, to=input$maxWindowClass, length.out=v$PParameterClass$nOperations), digits = 0)
  #print(v$PParameterClass)
  
  # Downsample the target
  aTarIndex <- seq(from = v$PParameterClass$Jump, to = v$AIData$Variables.nrow, by = v$PParameterClass$Jump)
 
  # Run accordion
  features <- embeddedGainRatioFS(v$data[aTarIndex,ncol(v$data)], v$data[1:aTarIndex[length(aTarIndex)],-ncol(v$data)], v$AIData, v$PParameterClass)
  print(summary(features))
  
  # Train decision tree and record accuracy
  treeModel <- J48(DData ~ ., data=features)
  print(summary(treeModel, numFolds = 10))
  # TODO: plot decision tree
  plot(treeModel,cex=0.5)

})

