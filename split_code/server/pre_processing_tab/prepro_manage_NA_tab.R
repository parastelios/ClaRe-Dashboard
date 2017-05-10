###################################################
#             Manage Missing Values               #
###################################################

# checking if target is Null, NA, numeric or not-numeric to use it for interpolate or repeating

output$targetIsNull <- reactive({
  targetIsNull = is.null(v$data[,ncol(v$data)])
  return(targetIsNull)
})
outputOptions(output, 'targetIsNull', suspendWhenHidden = FALSE)

createAlert(session, 'noMerged', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> The files are not merged ', 
            content = HTML('<p><b>Go to:</b>
                            <i>"Merge Tab"</i> <i class="fa fa-long-arrow-right" aria-hidden="true"></i>
                            <i><b>Choose</b> merge options and target variable and</i>
                            <i class="fa fa-long-arrow-right" aria-hidden="true"></i><i>"Merge"</i></p>'),
            append = F
            # style = 'warning'
            )

output$targetWithNoNAvalues <- reactive({
  targetWithNoNAvalues = (!anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetWithNoNAvalues)
})
outputOptions(output, 'targetWithNoNAvalues', suspendWhenHidden = FALSE)

createAlert(session, 'noNAs', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> 
                    The selected Target has no NA values. 
                    Continue to further <i>"Pre-processing"</i> or <i> Modeling (Regression/Classification)</i>',
            append = F
            # style = 'warning'
            )

output$isTargetNumeric <- reactive({
  isTargetNumeric = (is.numeric(v$data[,ncol(v$data)]) && anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(isTargetNumeric)
})
outputOptions(output, 'isTargetNumeric', suspendWhenHidden = FALSE)

output$isTargetNonNumeric <- reactive({
  isTargetNonNumeric = (!is.numeric(v$data[,ncol(v$data)]) && anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(isTargetNonNumeric)
})
outputOptions(output, 'isTargetNonNumeric', suspendWhenHidden = FALSE)

createAlert(session, 'targetIsNonNumeric', 
            title = "<i class='fa fa-info-circle' aria-hidden='true'></i> 
            The selected target is Non-Numeric, as a result <i>Repeat</i> NA's Policy should be used.",
            append = F
            # style = 'warning'
)

########## Interpolate ##########

# Event of clicking Interpolate
observeEvent(input$interpolate, {
  names = ncol(v$data)
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
  names = ncol(v$data)
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
  names = ncol(v$data)
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
