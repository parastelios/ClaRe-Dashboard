###################################################
#                Select Target                    #
###################################################  

# checking if no files are imported
output$noFileCheck <- reactive({
  noFile = is.null(v$singleData) & is.null(v$data_pre) & is.null(v$data_tar)
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
  oneFile = !is.null(v$singleData) & is.null(v$data_pre) & is.null(v$data_tar)
  return(oneFile)
})
outputOptions(output, 'oneFileCheck', suspendWhenHidden = FALSE)


# Event of clicking on confirm apply
observeEvent(input$confirmApply, {
  tarOption = input$targetOption0
  v$predictors = v$singleData[,-which(colnames(v$singleData) == tarOption)]
  v$target = v$singleData[,which(colnames(v$singleData) == tarOption)]
  v$selectedTarget = tarOption
  v$appliedData = bindData(v$predictors, v$target, input$excluding)
  v$data = v$appliedData
  v$mergedData = v$appliedData
  # v$d_colNA = getColWithNAEntries(v$data)
  renderMergedDataTable(v$data) 
  removeModal()
})

# press apply button
observeEvent(input$apply, {

  output$targetOption0 <- renderText(input$targetOption0)
  
  parseExcludingFieldsToHTMLTable0 <- function(x) {
    result = '<table class="table"><thead style="display: block;"><tr><th>Excluded fields:</th></tr></thead><tbody style=
    "height: 100px; overflow: auto; display: block">'
    for (i in x) {
      result = paste(result, '<tr><td>', i, '</td></tr>');
    }
    result = paste(result, '</tbody></table>')
    return(result)
  }
  
  if(is.null(input$excluding)){
    text = HTML("None")
    output$showVarsRemoved <- renderText(parseExcludingFieldsToHTMLTable0(text))
  }
  else{
    output$showVarsRemoved <- renderText(parseExcludingFieldsToHTMLTable0(input$excluding))
  }
  
  showModal(modalDialog(
    title = "Dataset Choices",
    div(
      style='color: red; text-align:center',
      tags$h4(htmlOutput("targetWarning"))
    ),
    div(
      tags$table(
        class="table",
        tags$thead(
          tags$th('Selected target:')
        ),
        tags$tbody(
          tags$tr(
            tags$td(textOutput('targetOption0'))
          )
        )
      )
    ),
    div(
      htmlOutput('showVarsRemoved')
    ),
    footer = tagList(
      modalButton("Cancel"),
      bsButton('confirmApply', 'Confirm', style = "success")
    ),
    easyClose = FALSE
  ))
})

bindData <- function(preData, tarData, excluding) {
  # bind 
  dPre = preData[,c(setdiff(colnames(preData), excluding))]
  dTar = tarData
  d_binded = cbind(dPre, dTar)
  colnames(d_binded)[ncol(d_binded)] <- v$selectedTarget
  return(d_binded)
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

# checking both one file and two files are imported
output$allFilesCheck <- reactive({
  allFiles = !is.null(v$singleData) & !is.null(v$data_pre) & !is.null(v$data_tar)
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
