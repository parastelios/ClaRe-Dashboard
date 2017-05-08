###################################################
#                Select Target                    #
###################################################  

# checking if no files are imported
output$noFileCheck <- reactive({
  noFile = is.null(v$data) & is.null(v$data_pre) & is.null(v$data_tar)
  return(noFile)
})
outputOptions(output, 'noFileCheck', suspendWhenHidden = FALSE)

# info modal
createAlert(session, 'noDataInfo', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> For more options:', 
            content = HTML('<p><b>Go to Import and Load Predictors and Target</b>'),
            append = F
            # style = 'warning'
)


# checking if one file is imported
output$oneFileCheck <- reactive({
  oneFile = !is.null(v$data) & is.null(v$data_pre) & is.null(v$data_tar)
  return(oneFile)
})
outputOptions(output, 'oneFileCheck', suspendWhenHidden = FALSE)

# Confirm options for merge
observeEvent(input$apply, {

  output$targetOption0 <- renderText(input$targetOption0)
  
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



# checking both one file and two files are imported
output$allFilesCheck <- reactive({
  allFiles = !is.null(v$data) & !is.null(v$data_pre) & !is.null(v$data_tar)
  return(allFiles)
})
outputOptions(output, 'allFilesCheck', suspendWhenHidden = FALSE)

# info modal
createAlert(session, 'allDataInfo', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> For more options Re-run Dashboard and', 
            content = HTML('<p><b>Load a Single OR Seperate sets of Predictors and Target not both Single and Seperated</b>'),
            append = F,
            style = 'warning'
)
