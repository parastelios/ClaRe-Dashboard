
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dygraphs)
library(DT)
library(ggplot2)
library(scales)
library(zoo)
library(imputeTS)
library(CORElearn)
library(RWeka)
library(rJava)

# change maximum file size from 5MB to 1GB
options(shiny.maxRequestSize = 1000*1024^2)

# global settings
DEBUG_UPLOAD_ON  = F
predictorFile = './data_arcodium/runPre_sub1.csv'
# predictorFile = './data_arcodium/snowPredictors.csv'
targetFile = './data_arcodium/runTar_sub1.csv'
# targetFile = './data_arcodium/snowTarget.csv'

# Load merged file by dir
# LOAD_MERGED = F
# mergedFile = './mergedFile.csv'

# Save model option by run
SAVE_MODEL = T

# variable names
AD_GENERAL = 'AD2016General'
MIN_PLOT_HEIGHT_EACH_VARIABLE = 20
PLOT_HEIGHT_ALL = 600

shinyServer(function(input, output, session) {
  # session data storage
  
  v <- reactiveValues()
  
  # init
  v$predictorImport <- 1
  v$targetImport <- 1
  v$plotHeight <- PLOT_HEIGHT_ALL
  
  # Loading the tabs to server.R from the split_code file
  source(file.path("split_code", "server", "server_import_tab.R"),  local = TRUE)$v
  #source(file.path("split_code", "server", "pre_processing_tab","prepro_datetime_tab.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "pre_processing_tab","prepro_merge_data_tab.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "pre_processing_tab","prepro_manage_NA_tab.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "pre_processing_tab","prepro_save_tab.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "pre_processing_tab","prepro_other_options_tab.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "pre_processing_tab","prepro_plot_tab1.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "model_tab", "model_tab.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "model_tab", "model_tab_classification.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "model_tab", "model_tab_regression.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "model_tab", "model_tab_saveFeaturesReg.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "model_tab", "model_tab_saveFeaturesClass.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "server_evaluation_tab0.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "server_evaluation_tab.R"),  local = TRUE)$v
  source(file.path("split_code", "server", "server_observe_obj.R"),  local = TRUE)$v
  # Load Accordion code (Class/Regres)
  source('rQuickie.R')
  source('featureSelection.R')
  source('featureConstruction.R')
  
  # Automatically stop a Shiny app when closing the browser tab (http://deanattali.com/blog/advanced-shiny-tips/)
  #session$onSessionEnded(stopApp)  
})
