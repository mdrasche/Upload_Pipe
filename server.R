library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(rdrop2)
source("functions/pre_process.R")
source("functions/outliers.R")
source("functions/score.R")
source("functions/mean_analysis.R")
source("functions/widget_helper_funcs.R")

options(shiny.maxRequestSize = 30*1024^2) # Set the maximum upload file size

shinyServer(function(input, output) {
  values <- reactiveValues() #
  values$cols <- c('Subject','Cond', 'Block', 'Cog', 'TrialType', 'ListLength', 'measure')
###Initial Calculations  
  clean_dat <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    #Load File
    dat <- read.csv(inFile$datapath, header = input$header,
                    sep = input$sep, quote = input$quote)
    #Preprocess Data
    dat <- preprocess(dat)
    reject_all <- outlier_detection(dat)
    clean(dat, reject_all)
  })
  scored_dat <- reactive({
    score(clean_dat())
  })
###Sidebar    
  #Create Checkbox depending on which listlengths are in data
  output$llOptions <- renderUI({
    options <- unique(clean_dat()$ListLength)
    choices <- optioncreator(options)
    checkboxGroupInput("llCheckBox", 
                       label = h3("Include"), 
                       choices = choices,
                       selected = 5)
  })
  
  
  ll <- reactive({
    #values$cols <- values$cols[values$cols != 'ListLength']
    if (length(input$llCheckbox) > 0) {
     # values$cols <- c(values$cols, 'ListLength')
      ll <- input$llCheckbox
    }
  })
  
  tt <- reactive({
    #values$cols <- values$cols[values$cols != 'TrialType']
    if (input$ttbox == 1) { 
      tt <- c("NP", "NN", "RN")
    }
    else{
     # values$cols <- c(values$cols, 'TrialType')
      tt <- input$ttbox
    }
  })
    

    mdat <- reactive({
      mean_trans(scored_dat(), values$cols, ll=ll(), tt=tt())
    }) 
    
    #values$mdat <- mean_trans(values$scored_dat, c(values$cols, c('TrialType', 'ListLength','measure')))
    #values$mdat_ll5ttNP <- mean_trans(values$scored_dat, c(values$cols, c('TrialType', 'ListLength','measure')), ll=5, tt="NP")
    #values$mdat_aggLL <- mean_trans(values$scored_dat, c(values$cols, c('TrialType','measure')))
    #values$mdat_aggtt <- mean_trans(values$scored_dat, c(values$cols, c('ListLength','measure')))
    #values$mdat_BlockNo <- mean_trans(values$scored_dat, c(values$cols, c('TrialType', 'ListLength','BlockNo','measure')))
    
    #Outlier Plots
    #values$p_out <- outlier_plot(output$dat, output$reject_all)
    #values$p_out_tt <- outlier_plot_tt(output$dat, output$reject_all)

  
  output$table <- renderTable({
    head(mdat())
    #cols <- colnames(values$mdat)[colnames(values$mdat) != 'Subject' & colnames(values$mdat) != 'dv']
    #ddply(values$mdat, cols, summarize, dv = mean(dv))
  })
  
  output$tTest <- renderTable({
    input$llCheckbox
    #TODO Add 'Probe.RT' and possibly other measures
    #TODO? RT transformation options
    #TODO Within vs. Between
    #t_list <- t.test.cog(values$mdat, 'Probe.ERR', within=T)
    #ldply(t_list$ERR, t.summary)
  })
  
  output$outlier_plot <- renderPlot({
    values$p_out
  })
  output$outlier_plot_tt <- renderPlot({
    values$p_out_tt
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('test', '.csv', sep='') },
    content = function(file) {
      write.csv(clean_dat(), file)
      drop_upload(file, dest = "drop_test")
    }
  )

})