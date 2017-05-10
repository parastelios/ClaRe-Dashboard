###################################################
#             Manage Missing Values               #
###################################################

# checking if target is Null, NA, numeric or not-numeric to use it for interpolate or repeating

output$targetIsNull0 <- reactive({
  targetIsNull0 = is.null(v$data[,ncol(v$data)])
  return(targetIsNull0)
})
outputOptions(output, 'targetIsNull0', suspendWhenHidden = FALSE)

createAlert(session, 'noTarget', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> No target is selected ', 
            content = HTML('<p><b>Go to:</b>
                           <i>"Select Target" tab</i> <i class="fa fa-long-arrow-right" aria-hidden="true"></i>
                           <i><b>Choose</b> target variable and</i>
                           <i class="fa fa-long-arrow-right" aria-hidden="true"></i><i>press "Apply"</i></p>'),
            append = F
            # style = 'warning'
            )

output$targetWithNoNAvalues0 <- reactive({
  targetWithNoNAvalues0 = (!anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetWithNoNAvalues0)
})
outputOptions(output, 'targetWithNoNAvalues0', suspendWhenHidden = FALSE)

createAlert(session, 'noNAs0', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> 
            The selected Target has no NA values. 
            Continue to further <i>"Pre-processing"</i> or <i> Modeling (Regression/Classification)</i>',
            append = F
            # style = 'warning'
)

output$isTargetNumeric0 <- reactive({
  isTargetNumeric0 = (is.numeric(v$data[,ncol(v$data)]) && anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(isTargetNumeric0)
})
outputOptions(output, 'isTargetNumeric', suspendWhenHidden = FALSE)

output$isTargetNonNumeric0 <- reactive({
  isTargetNonNumeric0 = (!is.numeric(v$data[,ncol(v$data)]) && anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(isTargetNonNumeric0)
})
outputOptions(output, 'isTargetNonNumeric0', suspendWhenHidden = FALSE)

createAlert(session, 'targetIsNonNumeric0', 
            title = "<i class='fa fa-info-circle' aria-hidden='true'></i> 
            The selected target is Non-Numeric, as a result <i>Repeat</i> NA's Policy should be used.",
            append = F
            # style = 'warning'
)

########## Interpolate ##########

# Event of clicking Interpolate
observeEvent(input$interpolate0, {
  names = ncol(v$data)
  method = input$interpolationMethod0
  v$data = interpolateColValues0(v$data, names, method)
  v$data = na.omit(v$data)
  renderMergedDataTable(v$data)
})

interpolateColValues0 <- function(array, colNameList, method) {
  for (i in colNameList) {
    array[,i] = na.interpolation(array[,i], option = method)
  }
  return(array)
}

########## Repeating ##########

# Event of repeating Numeric column
observeEvent(input$repeating01, {
  names = ncol(v$data)
  v$data = repeatingColValues01(v$data, names)
  v$data = na.omit(v$data)
  renderMergedDataTable(v$data)
})

repeatingColValues01 <- function(array, colNameList) {
  for (i in colNameList) {
    array[,i] = na.locf(array[,i], option = "locf", na.remaining = "rev")
  }
  return(array)
}

# Event of repeating NonNumeric column
observeEvent(input$repeating02, {
  names = ncol(v$data)
  v$data = repeatingColValues02(v$data, names)
  v$data = na.omit(v$data)
  renderMergedDataTable(v$data)
})

repeatingColValues02 <- function(array, colNameList) {
  for (i in colNameList) {
    array[,i] = na.locf.default(array[,i], na.rm = F)
    if(anyNA(array[,i])){
      array[,i] = na.locf.default(array[,i], fromLast = T)
    }
  }
  return(array)
}
