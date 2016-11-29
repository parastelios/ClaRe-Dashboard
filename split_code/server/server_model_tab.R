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
  # Updating model options(Class/Regres)
  source('rQuickie.R')
  source('featureSelection.R')
  source('featureConstruction.R')
  v$AIData <- analyseIndependentData(v$data_pre)
  v$AIData$Stable.Sampling.Rate <- "yes"
  
  # predictor rate
  v$preRate <- AIData$Sampling.Rate
  
  # target rate
  tarLocalSR <- 1/diff(v$data_tar[,input$targetField]/1000)
  meanTarSR <- signif(mean(tarLocalSR), 3)
  v$tarRate <- meanTarSR
  
  # update the regression options after merge
  updateNumericInput(session, "tarSampleRateReg",
                     value = v$tarRate, 
                     min = 0, max = v$preRate, step = 0.005)  
  maxWinReg <- v$preRate*(input$numOfSamplesReg)
  updateSliderInput(session, "maxWindowReg",
                    value = maxWinReg,
                    min = 0, max = 10*(maxWinReg), step = 5)
  v$PParameterReg <- parameterFinder(v$AIData$Sampling.Rate, v$tarRate, input$maxWindowReg)
  
  # update the classification options after merge
  updateNumericInput(session, "tarSampleRateClass", 
                     # label = 'Give Target sampling rate:',
                     value = v$tarRate, 
                     min = 0, max = v$preRate, step = 0.005)
  maxWinClass <- v$preRate*(input$numOfSamplesClass)
  updateSliderInput(session, "maxWindowClass",
                    value = maxWinClass,
                    min = 0, max = 10*(maxWinClass), step = 5)
  v$PParameterClass <- parameterFinder(v$AIData$Sampling.Rate, v$tarRate, input$maxWindowClass)
  print(v$PParameterClass)
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
  dataset <- embedded.CCF.FS(v$data_tar[,input$targetField], v$data_pre, v$AIData, v$PParameterReg)
  
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
  dataset <- embedded.IG.FS(v$data_tar[,input$targetField], v$data_pre, v$AIData, v$PParameterClass)
})

