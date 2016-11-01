###################################################
################# Other Options Tab ##############
###################################################

# click go button for normalizing
observeEvent(input$goNormalizing, {
  normalizingData(input$normalizing)
})

normalizingData <- function(normalizing){
  if(normalizing){
    # record the maximum for all varaibles to recover
    
    tempMax <- sapply(colnames(v$data),function(i){
      max(v$data[i])
    })
    tempMin <- sapply(colnames(v$data),function(i){
      min(v$data[i])
    })
    
    if(
      length(which(tempMax == 1)) == length(tempMax)    # all tempMax == 1 
      && length(which(tempMin == 0)) == length(tempMin) # all tempMin == 0 
    ){
      # it looks like that data has been normailized already
      # so keep previous max/min and skip normalizeTS (since it has no effect)
      v$data
    }
    else{
      v$dataMax <- tempMax
      v$dataMin <- tempMin
      v$data <- normalizeTS(v$data)
    }
  }
  # user choose to go to original data
  else{
    # recover by multipling the max values and min
    if(!is.null(v$dataMax)){
      for(i in colnames(v$data)){
        v$data[i] <- v$data[i] * (v$dataMax[i] - v$dataMin[i]) + v$dataMin[i]
      }
    }
  }
  return(v$data)
}

# remove excluding variables
observeEvent(input$confirmExcludingVar, {
  toggleModal(session, "popExcludingVar")
  v$data <- v$data[setdiff(colnames(v$data),input$excludingVar)]
})


# execute condition
observeEvent(input$goConditions, {
  
  # get index of rows according to the conditions
  aIndex <- do.call(input$equalCon, list(
    v$data[input$variableCon],
    input$numberCon
  )
  )
  
  # at least one element
  if(TRUE %in% aIndex){
    if(input$actionCon == "Remove line"){
      v$data <- v$data[!aIndex,] 
    }
    else if(input$actionCon == "Replace with"){
      v$data[aIndex, input$variableCon] <- input$replaceCon
    }
  }
  
  
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


observeEvent(input$confirmOutlierRemoval, {
  v$data <- v$data[-v$todoOutlierIndex, ]
})



# data preview
output$inputTable <- DT::renderDataTable({
  d <- v$data
  if (is.null(d))
    return()
  DT::datatable(d, options = list(pageLength = 20))
})

output$inputSummary <- renderPrint({
  d <- v$data
  if (is.null(d))
    return()
  summary(d)
})

output$processedDataset <- downloadHandler(
  filename = function() {
    paste('data-', Sys.time(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(v$data, file, row.names=FALSE)
  }
)