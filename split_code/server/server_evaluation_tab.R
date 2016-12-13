###################################################
#######           Evaluation Tab           ########
###################################################

# checking if features empty
output$isFeaturesEmpty <- reactive({
  isFeaturesEmpty = is.null(v$features)
  return(isFeaturesEmpty)
})
outputOptions(output, 'isFeaturesEmpty', suspendWhenHidden = FALSE)

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
  # TODO: plot predicted vs real target with dygraphs
  print(summary(predict(linearModel)))
})

# classification
observeEvent(input$goClass,{
  # Train decision tree and record accuracy
  treeModel <- J48(DData ~ ., data=v$features)
  print(summary(treeModel, numFolds = 10))

  output$featuresStatisticsSummary <- renderPrint({
    if (is.null(data))
      return()
    else
      summary(treeModel, numFolds = 10)
  })
  # TODO: plot decision tree
  plot(treeModel,cex=0.5)
})
