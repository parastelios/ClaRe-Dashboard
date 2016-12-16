###################################################
#            Save Features Regression             #
###################################################

# output save button
output$featuresRegDataset <- downloadHandler(
  filename = function() {
    paste('featuresReg-', Sys.time(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(v$features, file, row.names=FALSE)
  }
)


