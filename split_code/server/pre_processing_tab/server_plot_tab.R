###################################################
################# Plot Tab ########################
###################################################

output$plot <- renderDygraph({
  
  d <- v$data
  if (is.null(d)
      ||
      is.null(input$plotY)
  )
  {
    return()
  }
  
  # dygraph:
  # where the first element/column provides x-axis values and 
  # all subsequent elements/columns provide one or more series of y-values.
  
  # get selected X and Y
  
  if(input$plotX == 'DataIndex'){
    DataIndex <- seq(from = 1, to = nrow(d))
    target <- cbind(DataIndex, d[input$plotY])
  }
  else{
    target <- cbind(d[c(input$plotX,input$plotY)])
  }
  g <- dygraph(target)
  for(i in colnames(target)){
    if(is.na(as.numeric(target[1,i]))){
      g<- dyShading()
    }
  }
  
})


observeEvent(input$plPrev, {
  js$prevPl()
})

observeEvent(input$plNext, {
  js$nextPl()
})

observeEvent(input$plSave, {
  js$savePl()
})










# multiple plots
output$mulplot <- renderUI({

  d <- v$data  
  if(is.null(input$plotY)){
    return()
  }

  result_div <- div()

  out <- lapply(input$plotY, function(i){

    if(input$plotX == 'DataIndex'){
      DataIndex <- seq(from = 1, to = nrow(d))
      target <- cbind(DataIndex, d[i])
    }
    else{
      target <- d[c(input$plotX,i)]
    }

    # 1. create a container
    #     <div result/>
    #         <div for dygraph1/>
    #         <div for dygraph2/>
    #
    # 2. define output$dygraph1, output$dygraph2
    tempName <- paste0("mulplot_dygraph_", i)

    # output$xxx mush be defined before uiOutput("xxx") to make it work
    output[[tempName]] <- renderDygraph({
      dygraph(target, main = i, group = 'mulplot') %>%
        dyOptions(colors = "black")
    })
    dygraphOutput(tempName, width = "100%", height = "300px")

  })

  result_div <- tagAppendChild(result_div, out)

})

##################
# corelation plots
##################

output$corplot <- renderUI({

  d <- v$data
  if(is.null(input$plotY))
    return()

  result_div <- div()

  dop <- lapply(input$plotY, function(i){

    if(input$plotX == 'DataIndex'){
      DataIndex <- seq(from = 1, to = nrow(d))
      target <- cbind(DataIndex, d[i])
    }
    else{
      target <- d[c(input$plotX,i)]
    }

    tempName <- paste0("corplot_ggplot_", i)
    output[[tempName]] <- renderPlot({
      ggplot(target, aes_string(x = input$plotX, y = i)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        ggtitle(i)
    })
    plotOutput(tempName, width = "100%", height = "300px")

  })

  result_div <- tagAppendChild(result_div, dop)

})

