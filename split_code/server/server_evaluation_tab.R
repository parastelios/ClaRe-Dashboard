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
})

# classification
observeEvent(input$goClass,{
  # Train decision tree and record accuracy
  treeModel <- J48(DData ~ ., data=v$features)
  print(summary(treeModel, numFolds = 10))
  # print(treeModel)
  output$featuresStatisticsSummary <- renderPrint({
    if (is.null(data))
      return()
    else
      summary(treeModel, numFolds = 10)
  })
  # plot decision tree
  output$decisionTree <- renderPlot({
    plot(treeModel)
  })
})
