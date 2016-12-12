###################################################
#                   Plot Data                     #
###################################################

# checking if target is NonNumerical to enable target's classes

#if it has NA's
output$classPlotcheckNAs <- reactive({
  target = tail(colnames(v$data),1) # tail(colnames(v$data),1) is used in order to avoid re-input of target option  
  isTargetNonNumeric = (!is.numeric(v$data[1,target]) 
                        && sum(is.na(v$data[,target]))>0
                        && target %in% input$plotY)
  output$textClassWithNA <- renderText({
    n = target
    if(n=='Please select Target'){
      paste('Merge for Options')
    }
    else{
      paste("<b>", n, "</b>", ' Non-numeric with NA values, go to', "<i>", 'Manage missing values',"</i>", ' tab')
    }
  })
  return(isTargetNonNumeric)
})
outputOptions(output, 'classPlotcheckNAs', suspendWhenHidden = FALSE)

# if it doesn't have NA's
output$classPlotcheck <- reactive({
  target = tail(colnames(v$data),1) # tail(colnames(v$data),1) is used in order to avoid re-input of target option
  isTargetNonNumeric = (!is.numeric(v$data[1,target]) 
                        && !sum(is.na(v$data[,target]))>0
                        && target %in% input$plotY)
  output$textClassSelector <- renderText({
    n = input$targetOption
    if(n=='Please select Target'){
      paste('Merge for Options')
    }
    else{
      paste("Select Target's classes:")
    }
  })
  return(isTargetNonNumeric)
})
outputOptions(output, 'classPlotcheck', suspendWhenHidden = FALSE)


# Event of clicking on Plot button
observeEvent(input$preProsPlot, {
  x <- input$plotX
  y <- input$plotY
  get_preProsPlot(input$plotType, v$data, x, y)
})


get_preProsPlot <- function(type, data, varX, varY){
  
  colorMax = 9
  colors = RColorBrewer::brewer.pal(colorMax, "Pastel1")
  
  # checking the output: dygraphOutput for simplePlot / uiOutput for other
  output$simplePlotCheck <- reactive({
    simple = (input$plotType == 'simplePlot')
    return(simple)
  })
  outputOptions(output, 'simplePlotCheck', suspendWhenHidden = FALSE)
  
  output$multiPlotCheck <- reactive({
    multi = (input$plotType == 'multiPlot')
    return(multi)
  })
  outputOptions(output, 'multiPlotCheck', suspendWhenHidden = FALSE)
  
  output$corrPlotCheck <- reactive({
    corr = (input$plotType == 'corrPlot')
    return(corr)
  })
  outputOptions(output, 'corrPlotCheck', suspendWhenHidden = FALSE)
  
    # Simple Plot
    # if(type == 'simplePlot'){
      output$plot <- renderDygraph({
        d <- data
        if (is.null(d)
            ||
            is.null(varY)
        )
        {
          return()
        }
        # dygraph:
        # where the first element/column provides x-axis values and
        # all subsequent elements/columns provide one or more series of y-values.

        # get selected X and Y
        if(varX == 'DataIndex'){
          DataIndex <- seq(from = 1, to = nrow(d))
          target <- cbind(DataIndex, d[varY])
        }
        else{
          target <- cbind(d[c(varX,varY)])
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
            # shading
            classes = unique(l[,3])
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
    # }
  # Multi Plot
  # if(type == 'multiPlot'){
    output$multi <- renderUI({
      
      d <- data  
      if(is.null(varY)){
        return()
      }
      
      
      target_col = tail(colnames(d),1)
      nonNumericTarget = (!is.numeric(d[1,target_col]) 
                            && !sum(is.na(d[,target_col]))>0
                            && target_col %in% varY)
      
      
      result_div <- div()
      out <- lapply(varY, function(i){
        if(i==target_col && nonNumericTarget){
          return()
        }
        else{
          if(nonNumericTarget){
            if(varX == 'DataIndex'){
              DataIndex <- seq(from = 1, to = nrow(d))
              target <- cbind(DataIndex, d[c(i,target_col)])
            }
            else{
              target <- d[c(varX,i,target_col)]
            }  
          }
          else{
            if(varX == 'DataIndex'){
              DataIndex <- seq(from = 1, to = nrow(d))
              target <- cbind(DataIndex, d[i])
            }
            else{
              target <- d[c(varX,i)]
            }
          }
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
            dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)%>%
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
              classes = unique(l[,3])
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
  # }

  # corelation plots
  output$corr <- renderUI({
    
    d <- data
    if(is.null(varY))
      return()
    
    result_div <- div()
    
    dop <- lapply(varY, function(i){
      
      if(varX == 'DataIndex'){
        DataIndex <- seq(from = 1, to = nrow(d))
        target <- cbind(DataIndex, d[i])
      }
      else{
        target <- d[c(varX,i)]
      }
      tempName <- paste0("corplot_ggplot_", i)
      output[[tempName]] <- renderPlot({
        ggplot(target, aes_string(x = varX, y = i)) +
          geom_point() +
          geom_smooth(method = "lm", se = F) +
          ggtitle(i)
      })
      plotOutput(tempName, width = "100%", height = "300px")
      
    })
    result_div <- tagAppendChild(result_div, dop)
  })
}