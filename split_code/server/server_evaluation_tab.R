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
  model <- lm(DData ~ ., data=v$features)
  print(summary.lm(model)) 
  PParameterReg <- v$PParameterReg
  output$featuresStatisticsSummary <- renderPrint({
    if (is.null(data))
      return()
    else
      summary.lm(model)
  })
  v$predicted_target <- predict(model)
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
  print(model)
  output$exportModel <- downloadHandler(
    filename = function() {
      paste('RegressiontionModel-', Sys.time(), '.rda', sep='')
    },
    content = function(file) {
      save(model, PParameterReg, file = file)
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
  model <- J48(DData ~ ., data=v$features)
  .jcache(model$classifier)
  print(summary(model, numFolds = 10))
  PParameterClass <- v$PParameterClass
  # print(model)
  output$featuresStatisticsSummary <- renderPrint({
    if (is.null(data))
      return()
    else
      summary(model, numFolds = 10)
  })
  # plot decision tree
  # TODO: make it look nicer
  output$decisionTree <- renderPlot({
    plot( model)
  })
  
  # export model
  print(model)
  output$exportModel <- downloadHandler(
    filename = function() {
      paste('ClassificationModel-', Sys.time(), '.rda', sep='')
    },
    content = function(file) {
      save(model, PParameterClass, file = file)
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

#################################################
#        Import new Model and Data              #

# import Model
updatemodelInput <- function (name = NULL){
  output$loadModel <- renderUI({
    
    index <- isolate(v$loadModel) # re-render
    result <- div()
    
    result <- tagAppendChild(
      result,
      fileInput(paste0('modelFile', index), 
                'Choose Model File (.rda/.rds)',
                accept = c(
                  "text/rds",
                  "text/rda",
                  "text/RData",
                  "text/plain",
                  ".rda",
                  ".RData",
                  ".rds"
                )
      )
    )
    
    if(!is.null(name)){
      result <- tagAppendChild(
        result, 
        div(
          class="progress progress-striped",
          
          div(
            class="progress-bar",
            style="width: 100%",
            name, 
            " upload complete"
          )
        )
      )
    }
    
    result
    
  })
}

updatemodelInput()

# using reactive to dynamically import 
# model input
dataInputModel <- reactive({
  
  Model_inFile <- input[[paste0('modelFile', v$loadModel)]]
  
  if (is.null(Model_inFile)){
    # return(NULL)
    return(v$model)
  }
  
  load(Model_inFile$datapath)
  
  # modelFinal 
  if (is.null(model)){
    v$model <- NULL
  }
  else{
    v$model <- model
  }
  
  if (!is.null(v$model)){
    v$loadModel <- v$loadModel + 1
    updatemodelInput(name = Model_inFile$name)
  }
  
  # return model for display
  return(v$model)
})

# model preview
output$modelPreview <- renderPrint({
  model <- dataInputModel()
  
  if (is.null(model))
    return()
  
  summary(model)
})

# import model dataset
updatemodelDataInput <- function (name = NULL){
  output$loadModelData <- renderUI({
    
    index <- isolate(v$loadModelData) # re-render
    result <- div()
    
    result <- tagAppendChild(
      result,
      fileInput(paste0('modelDataFile', index), 
                'Choose Data File (.csv)',
                accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
    )
    
    if(!is.null(name)){
      result <- tagAppendChild(
        result, 
        div(
          class="progress progress-striped",
          
          div(
            class="progress-bar",
            style="width: 100%",
            name, 
            " upload complete"
          )
        )
      )
    }
    
    result
    
  })
}

updatemodelDataInput()

# using reactive to dynamically import dataset
# modeldata input
dataInputModelData <- reactive({
  
  ModelData_inFile <- input[[paste0('modelDataFile', v$loadModelData)]]
  
  if (is.null(ModelData_inFile)){
    # return(NULL)
    return(v$modelData)
  }
  
  d_model <- data.frame( 
    read.csv(
      ModelData_inFile$datapath, 
      header=input$modelHeader, 
      sep=input$modelSep,
      quote=input$modelQuote
    )
  )
  
  # dataFinal <- d
  v$modelData <- d_model
  
  if (
    is.null(d_model)
    || ncol(d_model) == 0
  ){
    v$data_model <- NULL
  }
  else{
    v$data_model <- d_model
  }
  
  if (!is.null(v$data_model)){
    v$loadModelData <- v$loadModelData + 1
    updatemodelDataInput(name = ModelData_inFile$name)
  }
  
  # return modelData for display
  return(v$modelData)
})

# table with navigation tab
# renderTable will kill the browser when is large

# modelData
output$modelDataTable <- renderUI({
  
  d_model <- dataInputModelData()
  
  if (is.null(d_model)){
    
    fluidRow(box(
      width = 12,
      background ="green",
      
      tags$h4(icon('bullhorn'),"Welcome"),
      HTML("Please upload a Model dataset to start.")
      
    ))
  }
  else{
    
    output$modelDataTable1 <- DT::renderDataTable({
      
      DT::datatable(d_model, options = list(pageLength = 20))
    })
    
    DT::dataTableOutput('modelDataTable1')
  }
})

# modelData summary
output$modelDataSummary <- renderPrint({
  d_model <- dataInputModelData()
  
  if (is.null(d_model))
    return()
  
  summary(d_model)
})

# Run loaded model
# Event of clicking on runModel0 button
observeEvent(input$runModel0, {
  
})