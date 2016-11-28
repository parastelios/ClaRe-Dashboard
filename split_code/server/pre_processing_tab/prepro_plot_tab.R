###################################################
################# Plot Tab ########################
###################################################

colorMax = 9
colors = RColorBrewer::brewer.pal(colorMax, "Pastel1")

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
  
  g <- dygraph(target)%>% 
    dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)
  
  # check which col have NA's
  varWithNA = colnames(target)[colSums(is.na(target)) > 0]

  # shading for the non-numerical Data
  for (n in colnames(target)) {
    if (is.numeric(target[1,n]) == F) {
      # create an array with the start and end index of every class 
      # using rle() function
      w = rle(as.vector(target[,n]))
      e=0
      l=c()
      for(i in 1:length(w$lengths)){
        s=e+1 #start point
        e=e+w$lengths[i] # end point
        # the array
        l=rbind(l,c(s,e,w$values[i]))
      }
      output$classPlotcheck <- reactive({
        output$textClassSelector <- renderText({
          paste("'",n,"' is non-numerical, Select class(es)")
        })
        return(TRUE)
      })
      outputOptions(output, 'classPlotcheck', suspendWhenHidden = FALSE)
      # shading 
      v$classes = unique(l[,3])
      if(is.null(input$plotClass)){
        return(g)
      }
      else{
        k = 1
        for(x in input$plotClass){
          for(j in 1:nrow(l)){
            if(is.na(l[j,3]) | is.na(x)){
              check = FALSE
            }
            else{
              check = (x == l[j,3]) 
            }
            if (check){
              g<-dyShading(g, from = l[j,1], to = l[j,2], color = colors[k %% colorMax+1])%>% 
                # dyAnnotation(l[j,1], l[j,3], attachAtBottom = TRUE, width = 60)%>% 
                dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)
            }
          }
          k = k + 1
        }
      }
    }
  }
  
  return(g)
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
      
      g <- dygraph(target, main = i, group = 'mulplot') %>%
            dyOptions(colors = "black")
      # check which col have NA's
      varWithNA = colnames(target)[colSums(is.na(target)) > 0]
      
      # shading for the non-numerical Data
      for (n in colnames(target)) {
        if (is.numeric(target[1,n]) == F) {
          # create an array with the start and end index of every class 
          # using rle() function
          w = rle(as.vector(target[,n]))
          e=0
          l=c()
          for(i in 1:length(w$lengths)){
            s=e+1 #start point
            e=e+w$lengths[i] # end point
            # the array
            l=rbind(l,c(s,e,w$values[i]))
          }
          
          # shading 
          v$classes = unique(l[,3])
          if(is.null(input$plotClass)){
            return(g)
          }
          else{
            k = 1
            for(x in input$plotClass){
              for(j in 1:nrow(l)){
                if(is.na(l[j,3]) | is.na(x)){
                  check = FALSE
                }
                else{
                  check = (x == l[j,3]) 
                }
                if (check){
                  g<-dyShading(g, from = l[j,1], to = l[j,2], color = colors[k %% colorMax+1])%>% 
                    # dyAnnotation(l[j,1], l[j,3], attachAtBottom = TRUE, width = 60)%>% 
                    dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)
                }
              }
              k = k + 1
            }
          }
        }
      }
      return(g)
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

