##################
# Regression tab #

# # checking if target is NULL
# output$targetisNullReg <- reactive({
#   targetisNullReg = (is.null(v$data[,ncol(v$data)]))
#   return(targetisNullReg)
# })
# outputOptions(output, 'targetStillWithNAReg', suspendWhenHidden = FALSE)

# checking if target has NAs
output$targetStillWithNAReg <- reactive({
  targetStillWithNAReg = (anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetStillWithNAReg)
})
outputOptions(output, 'targetStillWithNAReg', suspendWhenHidden = FALSE)

# checking if target is constant
output$targetConstantReg <- reactive({
  targetConstantReg = (length(unique(v$data[,ncol(v$data)]))==1 && !anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetConstantReg)
})
outputOptions(output, 'targetConstantReg', suspendWhenHidden = FALSE)

# checking if target can be predicted
output$targetWithoutNAReg <- reactive({
  targetWithoutNAReg = (!anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]) && !length(unique(v$data[,ncol(v$data)]))==1)
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
      output$featursDataRegTable0 <- DT::renderDataTable({
        DT::datatable(data, options = list(pageLength = 20))
      })
      DT::dataTableOutput('featursDataRegTable0')
    }
  })
  
  output$featuresRegDataSummary <- renderPrint({
    if (is.null(data))
      return()
    summary(data)
  })
}

# Plotting feature data


