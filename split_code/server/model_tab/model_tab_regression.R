##################
# Regression tab #

# # checking if target is NULL
# output$targetisNullReg <- reactive({
#   targetisNullReg = (is.null(v$data[,ncol(v$data)]))
#   return(targetisNullReg)
# })
# outputOptions(output, 'targetStillWithNAReg', suspendWhenHidden = FALSE)

# checking if target has NAs
output$targetStillWithNAReg <- reactive({
  targetStillWithNAReg = (anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetStillWithNAReg)
})
outputOptions(output, 'targetStillWithNAReg', suspendWhenHidden = FALSE)

# checking if target is constant
output$targetConstantReg <- reactive({
  targetConstantReg = (length(unique(v$data[,ncol(v$data)]))==1 && !anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]))
  return(targetConstantReg)
})
outputOptions(output, 'targetConstantReg', suspendWhenHidden = FALSE)

# checking if target can be predicted
output$targetWithoutNAReg <- reactive({
  targetWithoutNAReg = (!anyNA(v$data[,ncol(v$data)]) && !is.null(v$data[,ncol(v$data)]) && !length(unique(v$data[,ncol(v$data)]))==1)
  return(targetWithoutNAReg)
})
outputOptions(output, 'targetWithoutNAReg', suspendWhenHidden = FALSE)


# predictor sample rate
output$preSampleRateReg <- renderText({
  if (is.null(v$preRate)){
    return()
  }
  else{
    paste(v$preRate, 'Hz')
  }
})

# target sample rate is updated in merge_tab, while pressing merge button

# Adjusting max window size
observeEvent(input$numOfSamplesReg,{
  # Check preRate
  if(is.null(v$preRate)) {
    v$preRate <- 1
  }
  # print(v$preRate)
  v$maxWinReg <- v$preRate*(input$numOfSamplesReg)
  updateNumericInput(session, "maxWindowReg",
                     value = v$maxWinReg,
                     min = 0, max = 10*v$maxWinReg, step = 1)
})

# Running Accordion:
observeEvent(input$goReg,{
  v$modeling = 'regression'
  # Update sampling rate of predictors after pre-processing
  v$AIData <- analyseIndependentData(v$data[,-ncol(v$data)])
  v$AIData$Stable.Sampling.Rate <- "yes"
  
  # Update Accordion parameters according to the users preferences
  v$PParameterReg <- parameterFinder(v$AIData$Sampling.Rate, input$tarSampleRateReg, input$maxWindowReg)
  v$PParameterReg$nOperations <- input$numOfSamplesReg
  v$PParameterReg$Size <- round(seq(from=v$PParameterReg$Jump, to=input$maxWindowReg, length.out=v$PParameterReg$nOperations), digits = 0)
  
  # Downsample the target
  aTarIndex <- seq(from = v$PParameterReg$Jump, to = v$AIData$Variables.nrow, by = v$PParameterReg$Jump)
  aTarIndex[length(aTarIndex)]
  
  # Run accordion for lag regression
  if(input$regressionMethod == 'linearReg'){
    print('linear')
    v$features <- embedded.Cor.FS(v$data[aTarIndex,ncol(v$data)], v$data[1:aTarIndex[length(aTarIndex)],-ncol(v$data)], v$AIData, v$PParameterReg)
  }
  else{
    print('lag')
    v$features <- embedded.CCF.FS(v$data[aTarIndex,ncol(v$data)], v$data[1:aTarIndex[length(aTarIndex)],-ncol(v$data)], v$AIData, v$PParameterReg) 
  }

  print(summary(v$features))
  renderFeaturesRegDataTable(v$features)
})

# render feature data Reg function
renderFeaturesRegDataTable <- function(data) {
  output$featuresRegDataTable <- renderUI({
    # print(data)
    if (is.null(data)){
      fluidRow(box(
        width = 12,
        background ="red",
        tags$h4(icon('bullhorn'),"Features Data NULL!")
        #HTML("Please upload a dataset to start.")
      ))
    }
    else{
      output$featursDataRegTable0 <- DT::renderDataTable({
        DT::datatable(data, options = list(pageLength = 20))
      })
      DT::dataTableOutput('featursDataRegTable0')
    }
  })
  
  output$featuresRegDataSummary <- renderPrint({
    if (is.null(data))
      return()
    summary(data)
  })
}

###################################################
#                   Plot Features                 #
###################################################

# checking if target is NonNumerical to enable target's classes
output$classPlotcheckReg <- reactive({
  target = colnames(v$features)[1]
  isTargetNonNumericReg = (!is.numeric(v$features[,target]) 
                        && target %in% input$plotRegY)
  output$textClassSelectorReg <- renderText({
    paste("Select Target's classes:")
  })
  return(isTargetNonNumericReg)
})
outputOptions(output, 'classPlotcheckReg', suspendWhenHidden = FALSE)


# Event of clicking on Plot button
observeEvent(input$featuresRegPlot, {
  x <- input$plotRegX
  y <- input$plotRegY
  get_featuresRegPlot(input$featuresRegPlotType, v$features, x, y)
})


get_featuresRegPlot <- function(type, data, varX, varY){
  
  colorMax = 9
  colors = RColorBrewer::brewer.pal(colorMax, "Pastel1")
  
  # checking the output: dygraphOutput for simplePlot / uiOutput for other
  output$simplePlotCheckReg <- reactive({
    simpleReg = (input$featuresRegPlotType == 'simplePlotReg')
    return(simpleReg)
  })
  outputOptions(output, 'simplePlotCheckReg', suspendWhenHidden = FALSE)
  
  output$multiPlotCheckReg <- reactive({
    multiReg = (input$featuresRegPlotType == 'multiPlotReg')
    return(multiReg)
  })
  outputOptions(output, 'multiPlotCheckReg', suspendWhenHidden = FALSE)
  
  output$corrPlotCheckReg <- reactive({
    corrReg = (input$plotType == 'corrPlotReg')
    return(corrReg)
  })
  outputOptions(output, 'corrPlotCheckReg', suspendWhenHidden = FALSE)
  
  # Simple Plot
  output$plotReg <- renderDygraph({
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
      if (!is.numeric(target[,n])) {
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
        if(is.null(input$plotClassReg)){
          return(g)
        }
        else{
          k = 1
          for(x in input$plotClassReg){
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
  
  # Multi Plot
  output$multiReg <- renderUI({
    
    d <- data  
    if(is.null(varY)){
      return()
    }
    
    target_col = colnames(d)[1]
    nonNumericTargetReg = (!is.numeric(d[,target_col]) && target_col %in% varY)
    
    result_div <- div()
    out <- lapply(varY, function(i){
      if(i==target_col && nonNumericTargetReg){
        return()
      }
      else{
        if(nonNumericTargetReg){
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
      tempNameReg <- paste0("mulplot_dygraph_", i)
      
      # output$xxx mush be defined before uiOutput("xxx") to make it work
      output[[tempNameReg]] <- renderDygraph({
        
        g <- dygraph(target, main = i, group = 'mulplot') %>%
          dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)%>%
          dyOptions(colors = "black")
        
        # shading for the non-numerical Data
        for (n in colnames(target)) {
          if (!is.numeric(target[,n])) {
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
            if(is.null(input$plotClassReg)){
              return(g)
            }
            else{
              k = 1
              for(x in input$plotClassReg){
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
      dygraphOutput(tempNameReg, width = "100%", height = "300px")
      
    })
    
    result_div <- tagAppendChild(result_div, out)
    
  })
  # }
  
  # corelation plots
  output$corrReg <- renderUI({
    
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
      tempNameReg <- paste0("corplot_ggplot_", i)
      output[[tempNameReg]] <- renderPlot({
        ggplot(target, aes_string(x = varX, y = i)) +
          geom_point() +
          geom_smooth(method = "lm", se = F) +
          ggtitle(i)
      })
      plotOutput(tempNameReg, width = "100%", height = "300px")
      
    })
    result_div <- tagAppendChild(result_div, dop)
  })
}


