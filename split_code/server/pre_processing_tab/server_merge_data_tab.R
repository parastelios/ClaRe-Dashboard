###################################################
#                   Merge Data                    #
###################################################  
# dataPre = v$data_pre[,c(setdiff(colnames(v$d_pre), input$excludingPre))]
# dataTar = v$data_tar[,c(input$targetField,input$targetOption)]


# Event of clicking on merge button
observeEvent(input$confirmMerging, {
  predField = input$predictorField
  tarField = input$targetField
  tarOption = input$targetOption
  toggleModal(session, "popMerge")
  v$data = mergeData(v$data_pre, v$data_tar, predField, tarField, tarOption, input$excludingPre)
  v$d_colNA = getColWithNAEntries(v$data)
  renderMergedDataTable(v$data)
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
  output$showPredictorsRemoved <- renderText({
    paste("<b>","Predictors to be removed:", "</b>", toString(tempRemoveText))
  })
  div(
    style = "text-align:center",
    tags$h4(htmlOutput("showMergingVar")),
    tags$h4(htmlOutput("showTargetChoice")),
    tags$h4(htmlOutput("showPredictorsRemoved")),
    bsButton('confirmMerging', 'Confirm Merging', style = "danger")
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

# checking if cloWithNAvalues is numeric or not to use it for interpolate or repeating
output$mergecheck1 <- reactive({
  l1 = input$colWithNAvalues
  a1 = is.numeric(v$target[1,l1])
  # print(a1)
  # print("1")
  return(a1)
})
outputOptions(output, 'mergecheck1', suspendWhenHidden = FALSE)

output$mergecheck2 <- reactive({
  l2 = input$colWithNAvalues
  a2 = ! is.numeric(v$target[1,l2])
  # print(a2)
  # print("2")
  return(a2)
})
outputOptions(output, 'mergecheck2', suspendWhenHidden = FALSE)


