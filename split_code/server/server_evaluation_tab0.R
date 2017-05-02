###################################################
#######           Evaluation Tab 0         ########
###################################################
# info modal
createAlert(session, 'createModel', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> For more options:', 
            content = HTML('<p><b>Go to:</b>
                            <i>"Pre processing"</i> <i class="fa fa-long-arrow-right" aria-hidden="true"></i>
                            <i>"Merge"</i></p> 
                            <p>Choose your merge options/target variable and run <i>"Merge"</i></p>
                            <p><b> And then:</b></p>
                            <p><i> "Model(Regression or Classification)"</i></p>'),
            append = F
            # style = 'warning'
)

# import Model
updatemodelInput0 <- function (name = NULL){
  output$loadModel0 <- renderUI({
    
    index <- isolate(v$loadModel0) # re-render
    result <- div()
    
    result <- tagAppendChild(
      result,
      fileInput(paste0('modelFile0', index), 
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

updatemodelInput0()

# using reactive to dynamically import 
# model input
dataInputModel0 <- reactive({
  
  Model0_inFile <- input[[paste0('modelFile0', v$loadModel0)]]
  
  if (is.null(Model0_inFile)){
    # return(NULL)
    return(v$model)
  }
  
  load(Model0_inFile$datapath)
  
  # modelFinal 
  if (is.null(model)){
    v$model <- NULL
  }
  else{
    v$model <- model
  }
  
  if (!is.null(v$model)){
    v$loadModel0 <- v$loadModel0 + 1
    updatemodelInput0(name = Model0_inFile$name)
  }
  
  # return model for display
  return(v$model)
})

# model preview
output$modelPreview0 <- renderPrint({
  model <- dataInputModel0()
  
  if (is.null(model))
    return()
  
  summary(model)
  
})

# import model dataset
updatemodelDataInput0 <- function (name = NULL){
  output$loadModelData0 <- renderUI({
    
    index <- isolate(v$loadModelData0) # re-render
    result <- div()
    
    result <- tagAppendChild(
      result,
      fileInput(paste0('modelDataFile0', index), 
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

updatemodelDataInput0()

# using reactive to dynamically import dataset
# modeldata input
dataInputModelData0 <- reactive({
  
  ModelData0_inFile <- input[[paste0('modelDataFile0', v$loadModelData0)]]
  
  if (is.null(ModelData0_inFile)){
    # return(NULL)
    return(v$modelData)
  }
  
  d_model <- data.frame( 
    read.csv(
      ModelData0_inFile$datapath, 
      header=input$modelHeader0, 
      sep=input$modelSep0,
      quote=input$modelQuote0
    )
  )
  
  # dataFinal <- d
  v$modelData <- d_model

  if (
    is.null(d_model)
    || ncol(d_model) == 0
  ){
    v$data_model0 <- NULL
  }
  else{
    v$data_model0 <- d_model
  }
  
  if (!is.null(v$data_model0)){
    v$loadModelData0 <- v$loadModelData0 + 1
    updatemodelDataInput0(name = ModelData0_inFile$name)
  }
  
  # return modelData for display
  return(v$modelData)
})

# table with navigation tab
# renderTable will kill the browser when is large

# modelData
output$modelDataTable0 <- renderUI({
  
  d_model <- dataInputModelData0()
  
  if (is.null(d_model)){
    
    fluidRow(box(
      width = 12,
      background ="green",
      
      tags$h4(icon('bullhorn'),"Welcome"),
      HTML("Please upload a Model dataset to start.")
      
    ))
  }
  else{
    
    output$modelDataTable00 <- DT::renderDataTable({
      
      DT::datatable(d_model, options = list(pageLength = 20))
    })
    
    DT::dataTableOutput('modelDataTable00')
  }
})

# modelData summary
output$modelDataSummary0 <- renderPrint({
  d_model <- dataInputModelData0()
  
  if (is.null(d_model))
    return()
  
  summary(d_model)
  
})

# Run loaded model
# Event of clicking on runModel0 button
observeEvent(input$runModel0, {
  runModelClicked0 = TRUE
  # TODO: connect to recalculating algo
  # predict new labels
  # predict(model, newdata = testDataset)  
  # print(summary(model, newdata = testDataset))
  
  # check for results tabs
  output$isRunModelClicked0 <- reactive({
    isRunModelClicked0 = runModelClicked0
    return(isRunModelClicked0)
  })
  outputOptions(output, 'isRunModelClicked0', suspendWhenHidden = FALSE)
  
  # TODO: print results
})