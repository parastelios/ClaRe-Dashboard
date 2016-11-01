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





