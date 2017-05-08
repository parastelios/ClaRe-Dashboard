###################################################
#                   Merge Data                    #
###################################################  

# checking if two files are imported
output$twoFilesCheck <- reactive({
  twoFiles = !(is.null(v$data_pre) || is.null(v$data_tar)) & is.null(v$data)
  return(twoFiles)
})
outputOptions(output, 'twoFilesCheck', suspendWhenHidden = FALSE)

# Event of clicking on merge button
observeEvent(input$confirmMerging, {
  predField = input$predictorField
  tarField = input$targetField
  tarOption = input$targetOption
  v$selectedTarget = tarOption
  v$mergedData = mergeData(v$data_pre, v$data_tar, predField, tarField, tarOption, input$excludingPre)
  v$data = v$mergedData
  v$d_colNA = getColWithNAEntries(v$data)
  renderMergedDataTable(v$data) 
  removeModal()
})

# Confirm options for merge
observeEvent(input$merge, {
  output$predictorField <- renderText(input$predictorField)
  output$targetField <- renderText(input$targetField)
  output$targetOption <- renderText(input$targetOption)
  
  parseExcludingFieldsToHTMLTable <- function(x) {
    result = '<table class="table"><thead style="display: block;"><tr><th>Excluded fields:</th></tr></thead><tbody style=
    "height: 100px; overflow: auto; display: block">'
    for (i in x) {
      result = paste(result, '<tr><td>', i, '</td></tr>');
    }
    result = paste(result, '</tbody></table>')
    return(result)
  }
  
  if(is.null(input$excludingPre)){
    text = HTML("None")
    output$showPredictorsRemoved <- renderText(parseExcludingFieldsToHTMLTable(text))
  }
  else{
    output$showPredictorsRemoved <- renderText(parseExcludingFieldsToHTMLTable(input$excludingPre))
  }
  if(input$targetOption == input$targetField){
    output$targetWarning <- renderText({
      paste("Warning: Target is the same with the target Merge-field !")
    })
  }
  else{
    output$targetWarning <- renderText({
      paste('')
    })
  }
  showModal(modalDialog(
    title = "Merging Choices",
    div(
      style='color: red; text-align:center',
      tags$h4(htmlOutput("targetWarning"))
    ),
    div(
      tags$table(
        class="table",
        tags$thead(
          tags$th('Columns to be merged:')
        ),
        tags$tbody(
          tags$tr(
            tags$td(textOutput('predictorField'), textOutput('targetField'))
          )
        )
      ),
      tags$table(
        class="table",
        tags$thead(
          tags$th('Selected target:')
        ),
        tags$tbody(
          tags$tr(
            tags$td(textOutput('targetOption'))
          )
        )
      )
    ),
    div(
      htmlOutput('showPredictorsRemoved')
    ),
    footer = tagList(
      modalButton("Cancel"),
      bsButton('confirmMerging', 'Confirm Merging', style = "success")
    ),
    easyClose = FALSE
  ))
})

mergeData <- function(preData, tarData, fieldA, fieldB, target, excludingPredictors) {
  # merge
  dPre = preData[,c(setdiff(colnames(preData), excludingPredictors))]
  dTar = tarData[, c(fieldB, target)]
  # if (LOAD_MERGED == T) {
  #   d_merged = data.frame(read.csv(mergedFile))
  # }
  # else
    d_merged = merge(dPre, dTar, by.x=fieldA, by.y=fieldB, all=TRUE)
  # rename the columns with same name between Predictors and Target
  # sameColNames = intersect(colnames(dPre), colnames(dTar))
  # if(is.null(sameColNames) == FALSE){
  #   for (i in sameColNames) {
  #     colnames(d_merged)[colnames(d_merged)== i] <- paste0(i,"_Tar")
  #   }
  # }
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
