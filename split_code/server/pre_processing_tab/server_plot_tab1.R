###################################################
#                   Plot Data                     #
###################################################

# Event of clicking on Plot button
observeEvent(input$preProsPlot, {
  print('pressed')
  x <- input$plotX
  y <- input$plotY
  classes <- input$plotClass
  # data <- renderMergedDataTable(v$data)
  get_preProsPlot(input$plotType, v$data, x, y, classes)
  
})

get_preProsPlot <- function(type, data, varX, varY, class){
  print('in fun')
  colorMax = 9
  colors = RColorBrewer::brewer.pal(colorMax, "Pastel1")
  # output$plot <- renderUI({
    print('in plot')
    
    # Simple Plot
    if(type == 'simplePlot'){
      output$plot <- renderDygraph({
        print('in simple')
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
            output$classPlotcheck <- reactive({
              output$textClassSelector <- renderText({
                paste(n,'non-numerical, Select classes:')
              })
              return(TRUE)
            })
            outputOptions(output, 'classPlotcheck', suspendWhenHidden = FALSE)
            # shading
            v$classes = unique(l[,3])
            if(is.null(class)){
              return(g)
            }
            else{
              k = 1
              for(x in class){
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
    }
    # 
    # # Multi Plot
    # if(type == 'multiPlot'){
    #   print('in multi')
    #   d <- data  
    #   if(is.null(varY)){
    #     return()
    #   }
    #   
    #   result_div <- div()
    #   out <- lapply(varY, function(i){
    #     
    #     if(varX == 'DataIndex'){
    #       DataIndex <- seq(from = 1, to = nrow(d))
    #       target <- cbind(DataIndex, d[i])
    #     }
    #     else{
    #       target <- d[c(varX,i)]
    #     }
    #     
    #     # 1. create a container
    #     #     <div result/>
    #     #         <div for dygraph1/>
    #     #         <div for dygraph2/>
    #     #
    #     # 2. define output$dygraph1, output$dygraph2
    #     tempName <- paste0("mulplot_dygraph_", i)
    #     
    #     # output$xxx must be defined before uiOutput("xxx") to make it work
    #     output[[tempName]] <- renderDygraph({
    #       
    #       g <- dygraph(target, main = i, group = 'mulplot') %>%
    #         dyOptions(colors = "black")
    #       
    #       # check which col have NA's
    #       varWithNA = colnames(target)[colSums(is.na(target)) > 0]
    #       
    #       # shading for the non-numerical Data
    #       for (n in colnames(target)) {
    #         if (is.numeric(target[1,n]) == F) {
    #           # create an array with the start and end index of every class 
    #           # using rle() function
    #           w = rle(as.vector(target[,n]))
    #           e=0
    #           l=c()
    #           for(i in 1:length(w$lengths)){
    #             s=e+1 #start point
    #             e=e+w$lengths[i] # end point
    #             # the array
    #             l=rbind(l,c(s,e,w$values[i]))
    #           }
    #           output$classPlotcheck <- reactive({
    #             output$textClassSelector <- renderText({
    #               paste(n,' is non-numerical, Select classes to plot:')
    #             })
    #             return(TRUE)
    #           })
    #           outputOptions(output, 'classPlotcheck', suspendWhenHidden = FALSE)
    #           
    #           # shading 
    #           v$classes = unique(l[,3])
    #           if(is.null(class)){
    #             return(g)
    #           }
    #           else{
    #             k = 1
    #             for(x in class){
    #               for(j in 1:nrow(l)){
    #                 if(is.na(l[j,3]) | is.na(x)){
    #                   check = FALSE
    #                 }
    #                 else{
    #                   check = (x == l[j,3]) 
    #                 }
    #                 if (check){
    #                   g<-dyShading(g, from = l[j,1], to = l[j,2], color = colors[k %% colorMax+1])%>% 
    #                     # dyAnnotation(l[j,1], l[j,3], attachAtBottom = TRUE, width = 60)%>% 
    #                     dyLegend(show = "onmouseover", showZeroValues = TRUE, hideOnMouseOut = FALSE)
    #                 }
    #               }
    #               k = k + 1
    #             }
    #           }
    #         }
    #       }
    #       return(g)
    #     })
    #     dygraphOutput(tempName, width = "100%", height = "300px")
    #   })
    #   result_div <- tagAppendChild(result_div, out)
    # }
    # 
    # # corelation plots
    # if(type == 'corrPlot'){
    #   print('in cor')
    #   d <- data
    #   if(is.null(varY))
    #     return()
    #   
    #   result_div <- div()
    #   
    #   dop <- lapply(varY, function(i){
    #     
    #     if(varX == 'DataIndex'){
    #       DataIndex <- seq(from = 1, to = nrow(d))
    #       target <- cbind(DataIndex, d[i])
    #     }
    #     else{
    #       target <- d[c(varX,i)]
    #     }
    #     
    #     tempName <- paste0("corplot_ggplot_", i)
    #     output[[tempName]] <- renderPlot({
    #       ggplot(target, aes_string(x = varX, y = i)) +
    #         geom_point() +
    #         geom_smooth(method = "lm", se = F) +
    #         ggtitle(i)
    #     })
    #     plotOutput(tempName, width = "100%", height = "300px")
    #     
    #   })
    #   
    #   result_div <- tagAppendChild(result_div, dop)
    # }
  # })
}