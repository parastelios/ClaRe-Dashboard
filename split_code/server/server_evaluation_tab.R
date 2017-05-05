###################################################
#######           Evaluation Tab           ########
###################################################

# checking if features empty
output$isFeaturesEmpty <- reactive({
  isFeaturesEmpty = is.null(v$features)
  return(isFeaturesEmpty)
})
outputOptions(output, 'isFeaturesEmpty', suspendWhenHidden = FALSE)

# checking if features non empty
output$featuresNonEmpty <- reactive({
  featuresNonEmpty = !is.null(v$features)
  return(featuresNonEmpty)
})
outputOptions(output, 'featuresNonEmpty', suspendWhenHidden = FALSE)

# checking if regression
output$isRegression <- reactive({
  isRegression = is.numeric(v$features[,2])
  return(isRegression)
})
outputOptions(output, 'isRegression', suspendWhenHidden = FALSE)

# checking if classification
output$isClassification <- reactive({
  isClassification = !is.numeric(v$features[1,2])
  return(isClassification)
})
outputOptions(output, 'isClassification', suspendWhenHidden = FALSE)


# regression
observeEvent(input$goReg,{
  # build regression model
  colnames(v$features)[2] <- "target"
  model <- lm(target ~ ., data=v$features[,-1])
  v$model <- model
  print(summary.lm(v$model)) 
  PParameter <- v$PParameter
  output$featuresStatisticsSummary <- renderPrint({
    if (is.null(data))
      return()
    else
      summary.lm(v$model)
  })
  v$predictedTarget <- predict(model)
  
  # table with time, target and predicted target
  predTable <- cbind(v$features[,c(1,2)], v$predictedTarget)
  colnames(predTable)[2] <- v$selectedTarget
  colnames(predTable)[3] <- paste0('predicted_', v$selectedTarget)
  
  # mergedData with predicted target
  v$dataWithPrediction <- merge(v$mergedData, predTable[,c(1,3)], by.x = colnames(v$mergedData)[1],
                              by.y = colnames(predTable)[1], all = TRUE)
  
  renderMergedDataWithPred <- function(data) {
    output$mergedDataWithPred <- renderUI({
      
      if (is.null(v$dataWithPrediction)){
        fluidRow(box(
          width = 12,
          background ="red",
          tags$h4(icon('bullhorn'),"Merged Data NULL!")
          #HTML("Please upload a dataset to start.")
        ))
      }
      else{
        output$mergedDataWithPred0 <- DT::renderDataTable({
          DT::datatable(v$dataWithPrediction, options = list(pageLength = 20))
        })
        DT::dataTableOutput('mergedDataWithPred0')
      }
    })
    
    output$mergedDataWithPredSummary <- renderPrint({
      if (is.null(v$dataWithPrediction))
        return()
      summary(v$dataWithPrediction)
    })
  }
  
  # plot predicted vs real target with dygraphs
  output$targetTargetPlot <- renderDygraph({
    # TODO: Date-time in plotting!
    varX <- seq(from = 1, to = nrow(predTable))
    varY <- as.data.frame(predTable[,c(2,3)])
    g <- dygraph(cbind(varX, varY))%>%
      dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)
  })
  output$modelSummary <- renderPrint({
    summary(as.data.frame(predTable[,c(2,3)]))
  })
  
  # export model
  print(v$model)
  if (SAVE_MODEL == T) {
    save(model, PParameter, file = 'model.rda')
  }
  
  output$exportModel <- downloadHandler(
    filename = function() {
      paste('RegressiontionModel-', Sys.time(), '.rda', sep='')
    },
    content = function(file) {
      save(model, PParameter, file = file)
    }
  )

  target <- v$features[,c(1,2)]
  predictedTarget = v$predictedTarget
  dataModelReg <- as.data.frame(cbind(target, predictedTarget)) 
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
  colnames(v$features)[2] <- "target"
  model <- J48(target ~ ., data=v$features[,-1])
  v$model <- model
  .jcache(model$classifier)
  print(summary(model, numFolds = 10, seed = 17))
  PParameter <- v$PParameter
  output$featuresStatisticsSummary <- renderPrint({
    if (is.null(data))
      return()
    else
      summary(model, numFolds = 10, seed = 17)
  })
  
  # table with time, target and predicted target
  predTable <- cbind(v$features[,c(1,2)], v$predictedTarget)
  colnames(predTable)[2] <- v$selectedTarget
  colnames(predTable)[3] <- paste0('predicted_', v$selectedTarget)
  
  # mergedData with predicted target
  v$dataWithPrediction <- merge(v$mergedData, predTable[,c(1,3)], by.x = colnames(v$mergedData)[1],
                                by.y = colnames(predTable)[1], all = TRUE)
  
  renderMergedDataWithPred <- function(data) {
    output$mergedDataWithPred <- renderUI({
      
      if (is.null(v$dataWithPrediction)){
        fluidRow(box(
          width = 12,
          background ="red",
          tags$h4(icon('bullhorn'),"Merged Data NULL!")
          #HTML("Please upload a dataset to start.")
        ))
      }
      else{
        output$mergedDataWithPred0 <- DT::renderDataTable({
          DT::datatable(v$dataWithPrediction, options = list(pageLength = 20))
        })
        DT::dataTableOutput('mergedDataWithPred0')
      }
    })
    
    output$mergedDataWithPredSummary <- renderPrint({
      if (is.null(v$dataWithPrediction))
        return()
      summary(v$dataWithPrediction)
    })
  }
  
  # plot decision tree
  # TODO: make it look nicer
  output$decisionTree <- renderPlot({
    plot(model)
  })
  
  # export model
  print(v$model)
  plot(v$model)
  
  #save model in workspace and 
  if (SAVE_MODEL == T) {
    save(model, PParameter, file = 'model.rda')
  } 
  
  output$exportModel <- downloadHandler(
    filename = function() {
      paste('ClassificationModel-', Sys.time(), '.rda', sep='')
    },
    content = function(file) {
      save(model, PParameter, file = file)
    }
  )
  
  target <- v$features[,c(1,2)]
  predictedTarget = v$predictedTarget
  dataModelClass <- as.data.frame(cbind(target, predictedTarget)) 
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
#        Import new Data              #

# # checking for current model use
# output$currentModel <- reactive({
#   currentModel = !input$useCurrentModel
#   return(currentModel)
# })
# outputOptions(output, 'currentModel', suspendWhenHidden = FALSE)
# 
# 
# # import Model
# updatemodelInput <- function (name = NULL){
#   
#   output$loadModel <- renderUI({
#     
#     index <- isolate(v$loadModel) # re-render
#     result <- div()
#     
#     result <- tagAppendChild(
#       result,
#       fileInput(paste0('modelFile', index), 
#                 'Choose Model File (.rda/.rds)',
#                 accept = c(
#                   "text/rds",
#                   "text/rda",
#                   "text/RData",
#                   "text/plain",
#                   ".rda",
#                   ".RData",
#                   ".rds"
#                 )
#       )
#     )
#     
#     if(!is.null(name)){
#       result <- tagAppendChild(
#         result, 
#         div(
#           class="progress progress-striped",
#           
#           div(
#             class="progress-bar",
#             style="width: 100%",
#             name, 
#             " upload complete"
#           )
#         )
#       )
#     }
#     
#     result
#     
#   })
# }
# 
# updatemodelInput()
# 
# # using reactive to dynamically import 
# # model input
# dataInputModel <- reactive({
#   
#   Model_inFile <- input[[paste0('modelFile', v$loadModel)]]
#   
#   if (is.null(Model_inFile)){
#     return()
#     # return(v$model)
#   }
#   
#   load(Model_inFile$datapath)
#   
#   # modelFinal 
#   if (is.null(model)){
#     v$model <- NULL
#   }
#   else{
#     v$model <- model
#   }
#   
#   if (!is.null(v$model)){
#     v$loadModel <- v$loadModel + 1
#     updatemodelInput(name = Model_inFile$name)
#   }
#   
#   # return model for display
#   return(v$model)
# })
# 
# # model preview
# observeEvent(input$useCurrentModel,{
#   if (!input$useCurrentModel){
#     output$modelPreview <- renderPrint({
#       model <- dataInputModel()
#       
#       if (is.null(model))
#         return()
#       if (is.numeric(v$features[,2])) {
#         summary(model)  
#       }
#       else
#         summary(model, numFolds = 10, seed = 17)
#     })
#   }
#   else{
#     output$modelPreview <- renderPrint({
#       if (is.numeric(v$features[,2])) {
#         summary(v$model)
#       }
#       else
#         summary(v$model, numFolds = 10, seed = 17)
#     })
#   }
# })

# import model dataset
updatemodelDataInput <- function (name = NULL){
  output$loadModelData <- renderUI({
    
    index <- isolate(v$loadModelData) # re-render
    result <- div()
    
    result <- tagAppendChild(
      result,
      fileInput(paste0('modelDataFile', index), 
                'Choose File (.csv)',
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

# New model Data
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

# Run model with new Data
# check if RunModel is pressed for new prediction tab
v$runModelNOTClicked = TRUE
output$isRunModelNOTClicked <- reactive({
  isRunModelNOTClicked = v$runModelNOTClicked
  return(isRunModelNOTClicked)
})
outputOptions(output, 'isRunModelNOTClicked', suspendWhenHidden = FALSE)
# info modal
createAlert(session, 'clickRunModel', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> For more options:', 
            content = HTML('<p><b>Load a new dataset and press "Run Model" above </b>'),
            append = F
            # style = 'warning'
)

# check if RunModel is pressed for new Export Predicted Data tab
output$isRunModelNOTClicked1 <- reactive({
  isRunModelNOTClicked1 = v$runModelNOTClicked
  return(isRunModelNOTClicked1)
})
outputOptions(output, 'isRunModelNOTClicked1', suspendWhenHidden = FALSE)

# info modal
createAlert(session, 'clickRunModel1', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> For more options:', 
            content = HTML('<p><b>Load a new dataset and press "Run Model" above </b>'),
            append = F
            # style = 'warning'
)

# Event of clicking on runModel button
observeEvent(input$runModel,{
  v$runModelNOTClicked = FALSE
  # check if RunModel is pressed for new Export Predicted Data tab
  output$isRunModelClicked <- reactive({
    isRunModelClicked = !v$runModelNOTClicked
    return(isRunModelClicked)
  })
  outputOptions(output, 'isRunModelClicked', suspendWhenHidden = FALSE)
  # TODO: check if it has a dataset or it is empty
  # v$modelData the new dataset
  v$modelData <- v$modelData
  # TODO: check if it has the same columns or colNames with the one of the model
  
  # predictors
  IData <- v$modelData[,-ncol(v$modelData)]
  # TODO: choose target column

  # Analyse predictors
  AIData <- analyseIndependentData(IData)
  
  # Update Accordion parameters according to the users preferences
  PParameter <- v$PParameter

  # Downsample
  aTarIndex <- seq(from = PParameter$Jump, to = AIData$Variables.nrow, by = PParameter$Jump)
  
  # make sure that target and predictors are multiple of each other
  IData <- IData[1:aTarIndex[length(aTarIndex)],]
  target <- v$modelData[aTarIndex,ncol(v$modelData)]

  # build the evaluation dataset
  v$evalData <- data.frame(cbind(v$features[,1],target))
  print(v$evalData)
  for(k in 3:ncol(v$features)) {
  # start from col 3, because features start at this col, before them there are timestamp and DData cols
    FSName <- colnames(v$features)[k]
    FSTuple <- unlist(strsplit(FSName,split="_"))
    node <- eval(parse(text = paste(FSTuple[2],"(IData$",FSTuple[1],",AIData$Variables.nrow,PParameter$Jump,",FSTuple[3],")",sep="")))
    # v$evalData[[k]] <- node
    v$evalData <- cbind(v$evalData, node)
    colnames(v$evalData)[k] <- FSName
  }

  # predict new values
  print(summary(v$model, newdata = v$evalData[,-1]))
  v$predictedNewTarget <- predict(v$model, newdata = v$evalData[,-1])  
  
  # table with time, target and predicted target
  predTable <- cbind(v$evalData[,c(1,2)], v$predictedTarget)
  colnames(predTable)[2] <- v$selectedTarget
  colnames(predTable)[3] <- paste0('predicted_', v$selectedTarget)
  
  # mergedData with predicted target
  
  # plot predicted vs real target with dygraphs
  output$targetTargetPlotNew <- renderDygraph({
    # TODO: Date-time in plotting!
    varX <- seq(from = 1, to = nrow(v$evalData))
    varY <- as.data.frame(predTable[,c(2,3)])
    g <- dygraph(cbind(varX, varY))%>%
      dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)
  })
  output$newModelSummary <- renderPrint({
    summary(as.data.frame(predTable[,c(2,3)]))
  })
  
})

