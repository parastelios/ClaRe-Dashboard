###################################################
#                   Merge Data                    #
###################################################

# Event of clicking on merge button
observeEvent(input$merge, {
  predField = input$predictorField
  tarField = input$targetField
  v$data = mergeData(v$data_pre, v$data_tar, predField, tarField)
  v$d_colNA = getColWithNAEntries(v$data)
  renderMergedDataTable(v$data)
})

mergeData <- function(predictor, target, fieldA, fieldB) {
  d_merged = merge(predictor, target, by.x=fieldA, by.y=fieldB, all=TRUE)
  return(d_merged)
}

renderMergedDataTable <- function(data) {
  output$dataTable <- renderUI({
    
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
output$check1 <- reactive({
  l1 = input$colWithNAvalues
  a1 = is.numeric(v$target[1,l1])
  # print(a1)
  # print("1")
  return(a1)
})
outputOptions(output, 'check1', suspendWhenHidden = FALSE)

output$check2 <- reactive({
  l2 = input$colWithNAvalues
  a2 = ! is.numeric(v$target[1,l2])
  # print(a2)
  # print("2")
  return(a2)
})
outputOptions(output, 'check2', suspendWhenHidden = FALSE)


