###################################################
#             Manage Missing Values               #
###################################################

########## Interpolate ##########

# Event of clicking Interpolate
observeEvent(input$interpolate, {
  names = input$colWithNAvalues
  method = input$interpolationMethod
  v$data = interpolateColValues(v$data, names, method)
  v$data = na.omit(v$data)
  renderMergedDataTable(v$data)
})

interpolateColValues <- function(array, colNameList, method) {
  for (i in colNameList) {
    array[,i] = na.interpolation(array[,i], option = method)
  }
  return(array)
}

########## Repeating ##########

# Event of repeating Numeric column
observeEvent(input$repeating1, {
  names = input$colWithNAvalues
  v$data = repeatingColValues1(v$data, names)
  v$data = na.omit(v$data)
  renderMergedDataTable(v$data)
})

repeatingColValues1 <- function(array, colNameList) {
  for (i in colNameList) {
    array[,i] = na.locf(array[,i], option = "locf", na.remaining = "rev")
    }
  return(array)
}

# Event of repeating NonNumeric column
observeEvent(input$repeating2, {
  names = input$colWithNAvalues
  v$data = repeatingColValues2(v$data, names)
  v$data = na.omit(v$data)
  renderMergedDataTable(v$data)
})

repeatingColValues2 <- function(array, colNameList) {
  for (i in colNameList) {
    array[,i] = na.locf.default(array[,i])
    }
  return(array)
}
