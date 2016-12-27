###################################################
#######           Evaluation Tab           ########
###################################################

# checking if features empty
output$isFeaturesEmpty <- reactive({
  isFeaturesEmpty = is.null(v$features)
  return(isFeaturesEmpty)
})
outputOptions(output, 'isFeaturesEmpty', suspendWhenHidden = FALSE)

# checking if features empty
output$featuresNonEmpty <- reactive({
  featuresNonEmpty = !is.null(v$features)
  return(featuresNonEmpty)
})
outputOptions(output, 'featuresNonEmpty', suspendWhenHidden = FALSE)

# checking if regression
output$isRegression <- reactive({
  isRegression = is.numeric(v$features[,"DData"])
  return(isRegression)
})
outputOptions(output, 'isRegression', suspendWhenHidden = FALSE)

# checking if classification
output$isClassification <- reactive({
  isClassification = !is.numeric(v$features[1,"DData"])
  return(isClassification)
})
outputOptions(output, 'isClassification', suspendWhenHidden = FALSE)


# regression
observeEvent(input$goReg,{
  # build regression model
  linearModel <- lm(DData ~ ., data=v$features)
  print(summary.lm(linearModel)) 
  PParameterReg <- v$PParameterReg
  output$featuresStatisticsSummary <- renderPrint({
    if (is.null(data))
      return()
    else
      summary.lm(linearModel)
  })
  v$predicted_target <- predict(linearModel)
  # plot predicted vs real target with dygraphs
  output$targetTargetPlot <- renderDygraph({
    varX <- seq(from = 1, to = nrow(v$features))
    target <- v$features[,"DData"]
    predicted_target = v$predicted_target
    varY <- as.data.frame(cbind(target, predicted_target))
    g <- dygraph(cbind(varX, varY))%>%
      dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)
  })
  output$modelSummary <- renderPrint({
    target <- v$features[,"DData"]
    predicted_target = v$predicted_target
    summary(as.data.frame(cbind(target, predicted_target)))
  })
  
  # export model
  print(linearModel)
  output$exportModel <- downloadHandler(
    filename = function() {
      paste('RegressiontionModel-', Sys.time(), '.rda', sep='')
    },
    content = function(file) {
      save(linearModel, PParameterReg, file = file)
    }
  )
  # TODO:export data  plus timestamps
  target <- v$features[,"DData"]
  predicted_target = v$predicted_target
  dataModelReg <- as.data.frame(cbind(target, predicted_target)) 
  output$exportModelData <- downloadHandler(
    filename = function() {
      paste('RegModelData-', Sys.time(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dataModelReg, file, row.names=FALSE)
    }
  )
})

# classification
observeEvent(input$goClass,{
  # Train decision tree and record accuracy
  treeModel <- J48(DData ~ ., data=v$features)
  print(summary( treeModel, numFolds = 10))
  PParameterClass <- v$PParameterClass
  # print(treeModel)
  output$featuresStatisticsSummary <- renderPrint({
    if (is.null(data))
      return()
    else
      summary( treeModel, numFolds = 10)
  })
  # plot decision tree
  # TODO: make it look nicer
  output$decisionTree <- renderPlot({
    plot( treeModel)
  })
  
  # export model
  print(treeModel)
  output$exportModel <- downloadHandler(
    filename = function() {
      paste('ClassificationModel-', Sys.time(), '.rda', sep='')
    },
    content = function(file) {
      save(treeModel, PParameterClass, file = file)
    }
  )
  
  # TODO:export data plus timestamps
  target <- v$features[,"DData"]
  predicted_target = v$predicted_target
  dataModelClass <- as.data.frame(cbind(target, predicted_target)) 
  output$exportModelData <- downloadHandler(
    filename = function() {
      paste('ClassModelData-', Sys.time(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dataModelClass, file, row.names=FALSE)
    }
  )
})
