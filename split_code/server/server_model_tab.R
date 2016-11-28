# checking if target is null, numeric or not numeric to use it for the side menu (model, regression, classification)
output$selectmodeling <- renderMenu({
  target = tail(colnames(v$data),1)
  
  if(is.numeric(v$data[1,target])){
    menuItem("Regression", tabName = 'regression', icon = icon("th"))  
  }
  else if(! is.numeric(v$data[1,target]) && ! is.null(v$data[1,target])){
    menuItem("Classification", tabName = 'classification', icon = icon("th"))  
  }
  else{
    menuItem("Model", tabName = 'model', icon = icon("th"))  
  }
})
##################
# Regression tab #

# predictor sample rate
output$preSampleRateReg <- renderText({
  if (is.null(v$preRate)){
    return()
  }
  else{
    paste(v$preRate)
  }
})

# target sample rate is updated in merge_tab, while pressing merge button

# Adjusting max window size
observeEvent(input$numOfSamplesReg,{
  maxWinReg <- v$preRate*(input$numOfSamplesReg)
  updateSliderInput(session, "maxWindowReg",
                    value = maxWinReg,
                    min = 0, max = 10*(maxWinReg), step = 5)
  })

######################
# Classification tab #

# predictor sample rate
output$preSampleRateClass <- renderText({
  if (is.null(v$preRate)){
    return()
  }
  else{
    paste(v$preRate)
  }
})

# target sample rate is updated in merge_tab, while pressing merge button

# Adjusting max window size
observeEvent(input$numOfSamplesClass,{
  maxWinClass <- v$preRate*(input$numOfSamplesClass)
  updateSliderInput(session, "maxWindowClass",
                    value = maxWinClass,
                    min = 0, max = 10*(maxWinClass), step = 5)
  
})
