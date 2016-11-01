
###################################################
################# Import Tab  ####################
###################################################

###################################################
#              predictor file                     #
###################################################
updatePreFileInput <- function (name = NULL){
  output$predictorImport <- renderUI({
    
    index <- isolate(v$predictorImport) # re-render
    result <- div()
    
    result <- tagAppendChild(
      result,
      fileInput(paste0('prefile', index), 'Choose Predictor File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
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

updatePreFileInput()

# using reactive to dynamically import dataset
# predictor input
dataInputPredictor <- reactive({
  
  # upload debug
  if(DEBUG_UPLOAD_ON){
    d_pre <- data.frame( read.csv(predictorFile))  
    # d_pre <- d_pre[sapply(d_pre, is.numeric)]
    v$data_pre <- d_pre
    return(v$data_pre)
  } 
  
  pre_inFile <- input[[paste0('prefile', v$predictorImport)]]
  
  # TICKY PART:
  # 1. If initialized, `inFile` and `v$data_pre` are both `NULL`
  # 2. After each uploading, new `fileInput` is applied and
  #    we want to keep previous updated data_pre.
  #    It also prevent recursive creation of new `fileInput`s.
  
  if (is.null(pre_inFile)){
    # return(NULL)
    return(v$predictor)
  }
  
  d_pre <- data.frame( 
    read.csv(
      pre_inFile$datapath, 
      header=input$headerPre, 
      sep=input$sepPre,
      quote=input$quotePre
    )
  )
  
  # dataFinal <- d
  v$predictor <- d_pre
  
  # remove all non-numeric columns
  # d_pre <- d_pre[sapply(d_pre, is.numeric)]
  
  if (
    is.null(d_pre)
    || ncol(d_pre) == 0
  ){
    v$data_pre <- NULL
  }
  else{
    v$data_pre <- d_pre
    # comment out to disable normalization by default
    # and change ui:normailizing to FALSE
    # normalizingData(TRUE)
  }
  
  if (!is.null(v$data_pre)){
    v$predictorImport <- v$predictorImport + 1
    updatePreFileInput(name = pre_inFile$name)
  }
  
  # return predictorData for display
  return(v$predictor)
})



# data_pre table with navigation tab
# renderTable will kill the browser when the data_pre is large

# predictor
output$predictorTable <- renderUI({
  
  d_pre <- dataInputPredictor()
  
  if (is.null(d_pre)){
    
    fluidRow(box(
      width = 12,
      background ="green",
      
      tags$h4(icon('bullhorn'),"Welcome"),
      HTML("Please upload a Predictors dataset to start.")
      
    ))
  }
  else{
    
    output$predictorTable0 <- DT::renderDataTable({
      
      DT::datatable(d_pre, options = list(pageLength = 20))
    })
    
    DT::dataTableOutput('predictorTable0')
  }
})

# predictor data_pre summary
output$predictorSummary <- renderPrint({
  d_pre <- dataInputPredictor()
  
  if (is.null(d_pre))
    return()
  
  summary(d_pre)
  
})

###################################################
#                 target file                     #
###################################################
updateTarFileInput <- function (name = NULL){
  output$targetImport <- renderUI({
    
    index <- isolate(v$targetImport) # re-render
    result <- div()
    
    result <- tagAppendChild(
      result,
      fileInput(paste0('tarfile', index), 'Choose Target File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
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

updateTarFileInput()

# using reactive to dynamically import dataset
# target input
dataInputTarget <- reactive({
  
  # upload debug
  if(DEBUG_UPLOAD_ON){
    d_tar <- data.frame( read.csv(targetFile))  
    # d_tar <- d_tar[sapply(d_tar, is.numeric)]
    v$data_tar <- d_tar
    return(v$data_tar)
  } 
  
  tar_inFile <- input[[paste0('tarfile', v$targetImport)]]
  
  # TICKY PART:
  # 1. If initialized, `inFile` and `v$data_tar` are both `NULL`
  # 2. After each uploading, new `fileInput` is applied and
  #    we want to keep previous updated data_tar.
  #    It also prevent recursive creation of new `fileInput`s.
  
  if (is.null(tar_inFile)){
    # return(NULL)
    return(v$target)
  }
  
  d_tar <- data.frame( 
    read.csv(
      tar_inFile$datapath, 
      header=input$headerTar, 
      sep=input$sepTar,
      quote=input$quoteTar
    )
  )
  
  # dataFinal <- d_tar
  v$target <- d_tar
  
  # remove all non-numeric columns
  # d_tar <- d_tar[sapply(d_tar, is.numeric)]
  
  if (
    is.null(d_tar)
    || ncol(d_tar) == 0
  ){
    v$data_tar <- NULL
  }
  else{
    v$data_tar <- d_tar
    # comment out to disable normalization by default
    # and change ui:normailizing to FALSE
    # normalizingData(TRUE)
  }
  
  if (!is.null(v$data_tar)){
    v$targetImport <- v$targetImport + 1
    updateTarFileInput(name = tar_inFile$name)
  }
  
  # return targetData for display
  return(v$target)
})



# data_tar table with navigation tab
# renderTable will kill the browser when the data_tar is large

# target
output$targetTable <- renderUI({
  
  d_tar <- dataInputTarget()
  
  if (is.null(d_tar)){
    
    fluidRow(box(
      width = 12,
      background ="green",
      
      tags$h4(icon('bullhorn'),"Welcome"),
      HTML("Please upload a Target dataset to start.")
      
    ))
  }
  else{
    
    output$targetTable0 <- DT::renderDataTable({
      
      DT::datatable(d_tar, options = list(pageLength = 20))
    })
    
    DT::dataTableOutput('targetTable0')
  }
})

# target data_tar summary
output$targetSummary <- renderPrint({
  d_tar <- dataInputTarget()
  
  if (is.null(d_tar))
    return()
  
  summary(d_tar)
  
})

