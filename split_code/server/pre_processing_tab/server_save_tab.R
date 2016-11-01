###################################################
#            Save Preprocessed Data               #
###################################################


# output save button
output$processedDataset <- downloadHandler(
  filename = function() {
    paste('data-', Sys.time(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(v$data, file, row.names=FALSE)
  }
)