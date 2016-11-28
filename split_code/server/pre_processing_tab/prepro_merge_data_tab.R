###################################################
#                   Merge Data                    #
###################################################  

# Event of clicking on merge button
observeEvent(input$confirmMerging, {
  predField = input$predictorField
  tarField = input$targetField
  tarOption = input$targetOption
  toggleModal(session, "popMerge")
  v$data = mergeData(v$data_pre, v$data_tar, predField, tarField, tarOption, input$excludingPre)
  v$d_colNA = getColWithNAEntries(v$data)
  renderMergedDataTable(v$data)
 
  # Updating model options(Class/Regres)
  # predictor rate
  preLocalSR <- 1/diff(v$data_pre[,input$predictorField]/1000)
  meanPreSR <- signif(mean(preLocalSR), 3)
  v$preRate <- meanPreSR
  
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
  
  # update the classification options after merge
  updateNumericInput(session, "tarSampleRateClass", 
                     # label = 'Give Target sampling rate:',
                     value = v$tarRate, 
                     min = 0, max = v$preRate, step = 0.005)
  maxWinClass <- v$preRate*(input$numOfSamplesClass)
  updateSliderInput(session, "maxWindowClass",
                    value = maxWinClass,
                    min = 0, max = 10*(maxWinClass), step = 5)
  
})

# Confirm options for merge
output$uiMerging <- renderUI({
  tempPreFieldText <- input$predictorField
  tempTarFieldText <- input$targetField
  tempTargetChoiceText <- input$targetOption
  if(is.null(input$excludingPre)){
    tempRemoveText <- 'None'
  }
  else{
    tempRemoveText <- input$excludingPre
  }
  output$showMergingVar <- renderText({
    paste("<b>", "Columns to be merged:", "</b>", toString(c(tempTarFieldText, tempPreFieldText)))
  })
  output$showTargetChoice <- renderText({
    paste("<b>", "Selected Target:", "</b>", toString(tempTargetChoiceText)) 
  })
  if(input$targetOption == input$targetField){
    output$targetWarning <- renderText({
      paste("<b>", 'Warning: Target is the same with the target Merge-field !', "</b>")
    })
  }
  else{
    output$targetWarning <- renderText({
      paste('')
    })
  }
  output$showPredictorsRemoved <- renderText({
    paste("<b>","Predictors to be removed:", "</b>", toString(tempRemoveText))
  })
  div(
    style = "text-align:center",
    tags$h4(htmlOutput("showMergingVar")),
    tags$h4(htmlOutput("showTargetChoice")),
    tags$h4(htmlOutput("targetWarning")),
    tags$h4(htmlOutput("showPredictorsRemoved")),
    bsButton('confirmMerging', 'Confirm Merging', style = "primary")
  )
  
})


mergeData <- function(preData, tarData, fieldA, fieldB, target, excludingPredictors) {
  # merge
  dPre = preData[,c(setdiff(colnames(preData), excludingPredictors))]
  dTar = tarData[, c(fieldB, target)]
  d_merged = merge(dPre, dTar, by.x=fieldA, by.y=fieldB, all=TRUE)
  # rename the columns with same name between Predictors and Target
  sameColNames = intersect(colnames(dPre), colnames(dTar))
  if(is.null(sameColNames) == FALSE){
    for (i in sameColNames) {
      colnames(d_merged)[colnames(d_merged)== i] <- paste0(i,"_Tar")
    }
  }
  return(d_merged)
}

renderMergedDataTable <- function(data) {
  output$dataTable <- renderUI({
    # print(data)
    if (is.null(data)){
      fluidRow(box(
        width = 12,
        background ="red",
        tags$h4(icon('bullhorn'),"Merged Data NULL!")
        #HTML("Please upload a dataset to start.")
      ))
    }
    else{
      output$dataTable0 <- DT::renderDataTable({
        DT::datatable(data, options = list(pageLength = 20))
      })
      DT::dataTableOutput('dataTable0')
    }
  })
  
  output$dataSummary <- renderPrint({
    if (is.null(data))
      return()
    summary(data)
  })
}

####### Columns with NA #########
getColWithNAEntries <- function(array) {
  result = colnames(array)[colSums(is.na(array)) > 0]
  return(result)
}

# checking if target is numeric or not to use it for interpolate or repeating
output$isTargetNumeric <- reactive({
  isTargetNumeric = is.numeric(v$data[1,input$targetOption])
  return(isTargetNumeric)
})
outputOptions(output, 'isTargetNumeric', suspendWhenHidden = FALSE)

output$isTargetNonNumeric <- reactive({
  isTargetNonNumeric = !is.numeric(v$data[1,input$targetOption])
  return(isTargetNonNumeric)
})
outputOptions(output, 'isTargetNonNumeric', suspendWhenHidden = FALSE)

