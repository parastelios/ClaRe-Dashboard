###################################################
################# Other Options Tab ##############
###################################################

# remove excluding variables
observeEvent(input$confirmExcludingVar, {
  toggleModal(session, "popExcludingVar")
  v$data <- v$data[setdiff(colnames(v$data),input$excludingVar)]
  renderMergedDataTable(v$data)
})

output$uiExcludingVar <- renderUI({
  tempText <- input$excludingVar
  output$showExcludingVar <- renderText({
    if(is.null(tempText)){
      "No column selected"
    }
    else{
      paste("Going to remove:", toString(tempText))
    }
  })
  div(
    style = "text-align:center",
    tags$h4(textOutput("showExcludingVar")) ,
    bsButton('confirmExcludingVar', 'Confirm Excluding', style = "danger")
  )
})


# click go button for normalizing
observeEvent(input$goNormalizing, {
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

normalizingData <- function(normalizing){
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
observeEvent(input$confirmOutlierRemoval, {
  v$data <- v$data[-v$todoOutlierIndex, ]
  renderMergedDataTable(v$data)
  
})

# the content inside the popup
output$uiOutlierRemoval <- renderUI({
  # there will a plot showing which points to kill
  name <- input$outlierRemoval
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
                                 bsButton('confirmOutlierRemoval', 'Confirm Remove', style = "danger")
                               )
  )
  v$todoOutlierIndex <- aIndex
  v$todoOutlierName <- name
  result_div
})


# execute condition
# numerical
observeEvent(input$goConditions1, {
  # get index of rows according to the conditions
  aIndex1 <- do.call(input$equalCon1, 
                    list(
                      v$data[input$variableCon],
                      input$numberCon
                      )
                    )
  # at least one element
  if(TRUE %in% aIndex1){
    if(input$actionCon1 == "Remove line"){
      v$data <- v$data[!aIndex1,]
    }
    else if(input$actionCon1 == "Replace with"){
      v$data[aIndex1, input$variableCon] <- input$replaceCon1
    }
  }
  renderMergedDataTable(v$data)
})


# non numerical
observeEvent(input$goConditions2, {
  # get index of rows according to the conditions
  aIndex2 <- do.call(input$equalCon2, 
                     list(
                       v$data[input$variableCon],
                       input$textCon
                     )
  )
  # at least one element
  if(TRUE %in% aIndex2){
    if(input$actionCon2 == "Remove line"){
      v$data <- v$data[!aIndex2,]
    }
    else if(input$actionCon2 == "Replace with"){
      # replace class names
      aLevelIndex <- which(levels(v$data[, input$variableCon]) == input$textCon)
      levels(v$data[, input$variableCon])[aLevelIndex] <- input$replaceCon2
      v$data[aIndex2, input$variableCon] <- input$replaceCon2
    }
  }
  renderMergedDataTable(v$data)
})

# checking if conditionvalues is numeric or not 
output$condCheck1 <- reactive({
  l1 = input$variableCon
  a1 = is.numeric(v$data[1,l1]) && !(sum(is.na(v$data[,l1]))>0)
  # print(a1)
  # print(l1)
  return(a1)
})
outputOptions(output, 'condCheck1', suspendWhenHidden = FALSE)

output$condCheck2 <- reactive({
  l2 = input$variableCon
  a2 = ! is.numeric(v$data[1,l2]) && !(sum(is.na(v$data[,l2]))>0)
  # print(a2)
  # print(l2)
  return(a2)
})
outputOptions(output, 'condCheck2', suspendWhenHidden = FALSE)

output$condCheck3 <- reactive({
  l3 = input$variableCon
  if (sum(is.na(v$data[,l3]))>0) {
    a3 = TRUE
  }
  return(a3)
})
outputOptions(output, 'condCheck3', suspendWhenHidden = FALSE)

createAlert(session, 'conditionCheck3', 
            title = '<i class="fa fa-info-circle" aria-hidden="true"></i> The chosen variable has NA values.', 
            content = HTML('<p><b>Go to:</b> <i>"Manage missing values"</i> for options'),
            append = F
)
