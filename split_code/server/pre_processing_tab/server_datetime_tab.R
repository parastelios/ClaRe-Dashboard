###################################################
#####             Date/Time Tab               #####
###################################################


# data preview:
# predictor
output$predictor1Table <- renderUI({
  d_pre1 <- v$data_pre[1,]
  # print(d_pre1)
  if (is.null(d_pre1)){
    return()
    }
  else{
    output$predictor1Table0 <- DT::renderDataTable({
      DT::datatable(d_pre1, options = list(pageLength = 1,
                                           dom = 't'
                                           )
                    )
    })
    DT::dataTableOutput('predictor1Table0')
  }
})

# target
output$target1Table <- renderUI({
  d_tar1 <- v$data_tar[1,]
  # print(d_tar1)
  if (is.null(d_tar1)){
    return()
  }
  else{
    output$target1Table0 <- DT::renderDataTable({
      DT::datatable(d_tar1, options = list(pageLength = 1,
                                           dom = 't'
                                           )
                    )
    })
    DT::dataTableOutput('target1Table0')
  }
})


output$preTimestamp <- renderText({
  preTime <- v$data_pre[1,input$preTimeCol]
  # print(preTime)
  if (is.null(preTime)){
    return()
  }
  else{
    paste(preTime)
  }
})


output$tarTimestamp <- renderText({
  tarTime <- v$data_tar[1,input$tarTimeCol]
  # print(tarTime)
  if (is.null(tarTime)){
    return()
  }
  else{
    paste(tarTime)
  }
})

