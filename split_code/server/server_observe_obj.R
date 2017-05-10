
#########################################################
################ observe column names/numbers ###########
#########################################################
observe({
  
  d_preCol <- colnames(v$data_pre)
  d_tarCol <- colnames(v$data_tar)
  singleData_Col <- colnames(v$singleData)
  d_Col <- colnames(v$data)
  target <- tail(colnames(v$data),1)
  
  ################ Pre-processing tab ################
  ###########  date/time tab  ###########
  updateSelectInput(session, "preTimeCol", choices = d_preCol)
  updateSelectInput(session, "tarTimeCol", choices = d_tarCol)
  ########## select target tab ##########
  updateSelectInput(session, "targetOption0", choices = singleData_Col)
  updateSelectInput(session, "excluding", choices = singleData_Col)
  # updateSelectInput(session, "colWithNAvalues0", choices = v$d_colNA)
  ############## merge tab ##############
  updateSelectInput(session, "predictorField", choices = d_preCol)
  updateSelectInput(session, "targetField", choices = d_tarCol)
  updateSelectInput(session, "targetOption", choices = d_tarCol)
  updateSelectInput(session, "excludingPre", choices = d_preCol)
  updateSelectInput(session, "colWithNAvalues", choices = v$d_colNA)
  ########## other options tab0 #########
  # updateSelectInput(session, "excludingVar", choices = d_Col)
  # this will refresh all components
  # let's keep outlierRemoval as previous
  updateSelectInput(session, "excludingVar0", choices = d_Col)
  updateSelectInput(session, "outlierRemoval0", choices =  d_Col, selected = v$todoOutlierName)
  updateSelectInput(session, "variableCon0", choices = d_Col)
  ########## other options tab #########
  updateSelectInput(session, "excludingVar", choices = d_Col)
  updateSelectInput(session, "outlierRemoval", choices =  d_Col, selected = v$todoOutlierName)
  updateSelectInput(session, "variableCon", choices = d_Col)
  ############## plot tab ##############
  updateSelectInput(session, "plotY", choices = d_Col)
  updateSelectInput(session, "plotX", choices = c("DataIndex", d_Col))
  updateSelectInput(session, "plotClass", choices = levels(v$data[,target]))

  
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


################ Modeling tab ##################
observe({
  featuresCol <- colnames(v$features)
  target <- colnames(v$features)[1]
  
  ############## plotting Reg ##############
  updateSelectInput(session, "plotRegX", choices = c("DataIndex", featuresCol))
  updateSelectInput(session, "plotRegY", choices = featuresCol)
  updateSelectInput(session, "plotClassReg", choices = levels(v$features[,target]))  
  
  ############## plotting Class ##############
  updateSelectInput(session, "plotClassX", choices = c("DataIndex", featuresCol))
  updateSelectInput(session, "plotClassY", choices = featuresCol)
  updateSelectInput(session, "plotClassClass", choices = levels(v$features[,target]))  
})
