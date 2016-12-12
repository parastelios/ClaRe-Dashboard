###################################################
#          Save Features Classification           #
###################################################

# output save button
output$featuresClassDataset <- downloadHandler(
  filename = function() {
    paste('featuresClass-', Sys.time(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(v$features, file, row.names=FALSE)
  }
)