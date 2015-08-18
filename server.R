library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(rdrop2)
library(DT)
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
  col <- reactive({
    cols <- c('Subject','Cond', 'Block', 'TrialType', 'ListLength', 'measure')
    cols <- cols[!(cols %in%  input$aggregator)]
  })
  mdat <- reactive({
    mean_trans(scored_dat(), col())
  }) 
    
    
    #Outlier Plots
    #values$p_out <- outlier_plot(output$dat, output$reject_all)
    #values$p_out_tt <- outlier_plot_tt(output$dat, output$reject_all)

  output$mytable = renderDataTable({
    cols <- colnames(mdat())[!(colnames(mdat()) %in% c('Subject','dv'))]
    dat <- ddply(mdat(), cols, summarize, 
                 mean = round(mean(dv, na.rm = T),2), 
                 sd = round(sd(dv, na.rm = T),2))
    DT::datatable(dat, filter = 'top')
  })  
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