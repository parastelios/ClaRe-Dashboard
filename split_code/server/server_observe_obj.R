
#########################################################
################ observe column names/numbers ###########
#########################################################
observe({
  
  d_preCol <- colnames(v$data_pre)
  d_tarCol <- colnames(v$data_tar)
  

  ############## plot tab ##############
  updateSelectInput(session, "plotY", choices = c(d_preCol, d_tarCol))
  updateSelectInput(session, "plotX", choices = c("DataIndex", d_preCol, d_tarCol))
  
  ############## Pre-processing tab ##############
  updateSelectInput(session, "predictorField", choices = d_preCol)
  updateSelectInput(session, "targetField", choices = d_tarCol)
  updateSelectInput(session, "colWithNAvalues", choices = v$d_colNA)
  

  updateSelectInput(session, "excludingPreVar", choices = d_preCol)
  updateSelectInput(session, "excludingTarVar", choices = d_tarCol)
  # this will refresh all components
  # let's keep outlierRemoval as previous
  # TODO: Click the button, put the name in storage
  updateSelectInput(session, "outlierRemoval", choices = c(d_preCol, d_tarCol),selected = v$todoOutlierName)
  updateSelectInput(session, "variableCon", choices = c(d_preCol, d_tarCol))
  
 
  n_preCol <- ncol(v$data_pre)
  
  if(
    !is.null(n_preCol)
    &&
    n_preCol > PLOT_HEIGHT_ALL / MIN_PLOT_HEIGHT_EACH_VARIABLE
  ){
    v$plotHeight <- n_preCol * MIN_PLOT_HEIGHT_EACH_VARIABLE
  }
  
  n_tarCol <- ncol(v$data_tar)

  if(
    !is.null(n_tarCol)
    &&
    n_tarCol > PLOT_HEIGHT_ALL / MIN_PLOT_HEIGHT_EACH_VARIABLE
  ){
    v$plotHeight <- n_tarCol * MIN_PLOT_HEIGHT_EACH_VARIABLE
  }
  
})

# Conditions
observe({
  # get index of rows according to the conditions
  rowSelected <- tryCatch({
    aIndex <- do.call(input$equalCon, list(
      v$data_pre[input$variableCon],
      input$numberCon
    )
    )
    paste0(sum(aIndex)," rows selected")
  },
  error = function(error){
    return('')
  })
  
  output$rowSelected <- renderText({
    rowSelected
  })
})