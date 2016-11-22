
#########################################################
################ observe column names/numbers ###########
#########################################################
observe({
  
  d_preCol <- colnames(v$data_pre)
  d_tarCol <- colnames(v$data_tar)
  d_Col <- colnames(v$data)
  target <- tail(colnames(v$data),1)
  
  ################ Pre-processing tab ################
  ###########  date/time tab  ###########
  updateSelectInput(session, "preTimeCol", choices = d_preCol)
  updateSelectInput(session, "tarTimeCol", choices = d_tarCol)
  ############## merge tab ##############
  updateSelectInput(session, "predictorField", choices = d_preCol)
  updateSelectInput(session, "targetField", choices = d_tarCol)
  updateSelectInput(session, "targetOption", choices = d_tarCol)
  updateSelectInput(session, "excludingPre", choices = d_preCol)
  updateSelectInput(session, "colWithNAvalues", choices = v$d_colNA)
  ############## plot tab ##############
  updateSelectInput(session, "plotY", choices = d_Col)
  updateSelectInput(session, "plotX", choices = c("DataIndex", d_Col))
  updateSelectInput(session, "plotClass", choices = levels(v$data[,target]))
  ########## other optopns tab #########
  # updateSelectInput(session, "excludingVar", choices = d_Col)
  # this will refresh all components
  # let's keep outlierRemoval as previous
  # TODO: Click the button, put the name in storage
  updateSelectInput(session, "excludingVar", choices = d_Col)
  updateSelectInput(session, "outlierRemoval", choices =  d_Col, selected = v$todoOutlierName)
  updateSelectInput(session, "variableCon", choices = d_Col)
  # updateSelectInput(session, "textCon", choices = variableCon)
 
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
  rowSelected1 <- tryCatch({
    aIndex1 <- do.call(input$equalCon1, list(
      v$data[input$variableCon],
      input$numberCon
    )
    )
    paste0(sum(aIndex1)," rows selected")
  },
  error = function(error){
    return('')
  })
  output$rowSelected1 <- renderText({
    rowSelected1
  })
})

observe({
  # get index of rows according to the conditions
  rowSelected2 <- tryCatch({
    aIndex2 <- do.call(input$equalCon2, list(
      v$data[input$variableCon],
      input$textCon
    )
    )
    paste0(sum(aIndex2)," rows selected")
  },
  error = function(error){
    return('')
  })
  output$rowSelected2 <- renderText({
    rowSelected2
  })
})