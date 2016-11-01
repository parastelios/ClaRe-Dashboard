###################################################
#             Manage Missing Values               #
###################################################


########## Interpolate ##########

# Event of clicking Interpolate
observeEvent(input$interpolate, {
  names = input$colNAInterpolate
  method = input$interpolationMethod
  v$data = interpolateColValues(v$data, names, method)
  renderMergedDataTable(v$data)
  #print(names)
})

interpolateColValues <- function(array, colNameList, method) {
  for (i in colNameList) {
    array[,i] = na.interpolation(array[,i], option = method)
  }
  return(array)
}

########## Repeating ##########

# Event of repeating
observeEvent(input$repeating, {
  names = input$colNARepeating
  v$data = repeatingColValues(v$data, names)
  renderMergedDataTable(v$data)
})

repeatingColValues <- function(array, colNameList) {
  for (i in colNameList) {
    array[,i] = na.locf.default(array[,i], option = "locf", na.remaining = "keep") 
  }
  return(array)
}
