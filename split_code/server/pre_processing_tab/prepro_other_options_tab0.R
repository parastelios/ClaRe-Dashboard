###################################################
#      Other Options Tab Single file              #
###################################################

# remove excluding variables
observeEvent(input$confirmExcludingVar0, {
  toggleModal(session, "popExcludingVar0")
  v$data <- v$data[setdiff(colnames(v$data),input$excludingVar0)]
  renderMergedDataTable(v$data)
})

output$uiExcludingVar0 <- renderUI({
  tempText <- input$excludingVar0
  output$showExcludingVar0 <- renderText({
    if(is.null(tempText)){
      "No column selected"
    }
    else{
      paste("Going to remove:", toString(tempText))
    }
  })
  div(
    style = "text-align:center",
    tags$h4(textOutput("showExcludingVar0")) ,
    bsButton('confirmExcludingVar0', 'Confirm Excluding', style = "danger")
  )
})


# click go button for normalizing
observeEvent(input$goNormalizing0, {
  v$data <- normalizingData(input$normalizing)
  renderMergedDataTable(v$data)
  
})

# normalize function
normalizeTS <- function(data) {
  for(j in 1:ncol(data)) {
    # check numeric
    if(sapply(data[j], is.numeric)){
      minData <- min(data[,j], na.rm=TRUE)
      maxData <- max(data[,j], na.rm=TRUE)
      
      if(minData < maxData) {
        data[,j] <- (data[,j]-minData)/(maxData-minData)
      }
      # if minData equals maxData exactly
      else{
        data[j] <- 1
      }
    }
  }
  return(data)
}

normalizingData0 <- function(normalizing){
  # record the maximum for all varaibles to recover
  tempMax <- sapply(colnames(v$data),function(i){
    if (is.numeric(v$data[1,i]) == T) {
      max(v$data[i])
    }
  })
  tempMin <- sapply(colnames(v$data),function(i){
    if (is.numeric(v$data[1,i]) == T) {
      min(v$data[i])
    }
  })
  v$dataMax <- tempMax
  v$dataMin <- tempMin
  v$data <- normalizeTS(v$data)
  return(v$data)
}


# Outlier remover
observeEvent(input$confirmOutlierRemoval0, {
  v$data <- v$data[-v$todoOutlierIndex, ]
  renderMergedDataTable(v$data)
  
})

# the content inside the popup
output$uiOutlierRemoval0 <- renderUI({
  # there will a plot showing which points to kill
  name <- input$outlierRemoval0
  X <- v$data[name]
  
  avgX <- mean(X[,])
  diffX <- abs(X - avgX)
  aIndex <- which(diffX == max(diffX))
  
  plotName <- paste0('outlierRemoval', name)
  
  output[[plotName]] <- renderDygraph({
    graph <- dygraph(cbind(seq(from = 1, to = nrow(X)), X), main = name) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", drawGrid = FALSE) %>%
      dyLimit(avgX, paste0("Average of ",name),  labelLoc = "right", color = "black", strokePattern = "dotted")
    
    for(i in aIndex){
      graph <- dyEvent(graph, i, labelLoc = "bottom", color = "#DF4A32",  strokePattern = "solid")
    }
    for(i in unique(X[aIndex,])){
      graph <- dyLimit(graph, i, paste0("Farthest from average: ",i),  labelLoc = "right", color = "red", strokePattern = "dotted")
    }
    
    graph
    
  })
  
  
  result_div <- div()
  
  dop <- dygraphOutput(plotName, width = "100%", height = "300px")
  result_div <- tagAppendChild(result_div, dop)
  result_div <- tagAppendChild(result_div,
                               # div(
                               #
                               div(
                                 style="padding: 20px 0 10px; text-align:center;",
                                 tags$h4(
                                   tags$strong(length(aIndex))," row(s) selected, with value of ",
                                   tags$strong( toString(unique(X[aIndex,])) )
                                 ),
                                 bsButton('confirmOutlierRemoval0', 'Confirm Remove', style = "danger")
                               )
  )
  v$todoOutlierIndex <- aIndex
  v$todoOutlierName <- name
  result_div
})


# execute condition
# numerical
observeEvent(input$goConditions01, {
  # get index of rows according to the conditions
  aIndex1 <- do.call(input$equalCon01, 
                     list(
                       v$data[input$variableCon0],
                       input$numberCon0
                     )
  )
  # at least one element
  if(TRUE %in% aIndex1){
    if(input$actionCon01 == "Remove line"){
      v$data <- v$data[!aIndex1,]
    }
    else if(input$actionCon01 == "Replace with"){
      v$data[aIndex1, input$variableCon0] <- input$replaceCon01
    }
  }
  renderMergedDataTable(v$data)
})


# non numerical
observeEvent(input$goConditions02, {
  # get index of rows according to the conditions
  aIndex2 <- do.call(input$equalCon02, 
                     list(
                       v$data[input$variableCon0],
                       input$textCon0
                     )
  )
  # at least one element
  if(TRUE %in% aIndex2){
    if(input$actionCon02 == "Remove line"){
      v$data <- v$data[!aIndex2,]
    }
    else if(input$actionCon02 == "Replace with"){
      # replace class names
      aLevelIndex <- which(levels(v$data[, input$variableCon0]) == input$textCon0)
      levels(v$data[, input$variableCon0])[aLevelIndex] <- input$replaceCon02
      v$data[aIndex2, input$variableCon0] <- input$replaceCon02
    }
  }
  renderMergedDataTable(v$data)
})

# checking if conditionvalues is numeric or not 
output$condCheck01 <- reactive({
  l1 = input$variableCon0
  a1 = is.numeric(v$data[1,l1]) && !(sum(is.na(v$data[,l1]))>0)
  # print(a1)
  # print(l1)
  return(a1)
})
outputOptions(output, 'condCheck01', suspendWhenHidden = FALSE)

output$condCheck02 <- reactive({
  l2 = input$variableCon0
  a2 = ! is.numeric(v$data[1,l2]) && !(sum(is.na(v$data[,l2]))>0)
  # print(a2)
  # print(l2)
  return(a2)
})
outputOptions(output, 'condCheck02', suspendWhenHidden = FALSE)

output$condCheck03 <- reactive({
  l3 = input$variableCon0
  if (sum(is.na(v$data[,l3]))>0) {
    a3 = TRUE
  }
  return(a3)
})
outputOptions(output, 'condCheck03', suspendWhenHidden = FALSE)

createAlert(session, 'conditionCheck03', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> The chosen variable has NA values.', 
            content = HTML('<p><b>Go to:</b> <i>"Manage missing values"</i> for options'),
            append = F
)

# Conditions
observe({
  # get index of rows according to the conditions
  rowSelected01 <- tryCatch({
    aIndex1 <- do.call(input$equalCon01, list(
      v$data[input$variableCon0],
      input$numberCon0
    )
    )
    paste0(sum(aIndex1)," rows selected")
  },
  error = function(error){
    return('')
  })
  output$rowSelected01 <- renderText({
    rowSelected01
  })
})

observe({
  # get index of rows according to the conditions
  rowSelected02 <- tryCatch({
    aIndex2 <- do.call(input$equalCon02, list(
      v$data[input$variableCon0],
      input$textCon0
    )
    )
    paste0(sum(aIndex2)," rows selected")
  },
  error = function(error){
    return('')
  })
  output$rowSelected02 <- renderText({
    rowSelected02
  })
})
