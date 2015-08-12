library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(rdrop2)
source("functions/pre_process.R")
source("functions/outliers.R")
source("functions/score.R")
source("functions/mean_analysis.R")

options(shiny.maxRequestSize = 30*1024^2) # Set the maximum upload file size

shinyServer(function(input, output) {
  values <- reactiveValues()
  observe({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat <- read.csv(inFile$datapath, header = input$header,
                    sep = input$sep, quote = input$quote)
    values$dat <- preprocess(dat)
    values$reject_all <- outlier_detection(values$dat)
    values$clean_dat <- clean(values$dat,values$reject_all)
    values$scored_dat <- score(values$clean_dat)
    values$cols <- c('Subject','Cond', 'Block', 'Cog')
    values$mdat <- mean_trans(values$scored_dat, c(values$cols, c('TrialType', 'ListLength','measure')))
    values$mdat_ll5ttNP <- mean_trans(values$scored_dat, c(values$cols, c('TrialType', 'ListLength','measure')), ll=5, tt="NP")
    values$mdat_aggLL <- mean_trans(values$scored_dat, c(values$cols, c('TrialType','measure')))
    values$mdat_aggtt <- mean_trans(values$scored_dat, c(values$cols, c('ListLength','measure')))
    values$mdat_BlockNo <- mean_trans(values$scored_dat, c(values$cols, c('TrialType', 'ListLength','BlockNo','measure')))
    
    #Outlier Plots
    values$p_out <- outlier_plot(values$dat, values$reject_all)
    values$p_out_tt <- outlier_plot_tt(values$dat, values$reject_all)
  })
  output$table <- renderTable({
    cols <- c(values$cols, 'TrialType', 'ListLength', 'measure')
    if (input$DS_llbox == 5) { 
      ll <- 5
    }
    else{
      ll <- c(2:8)
      cols <- cols[cols != 'ListLength']
    }
    if (input$DS_ttbox == 1) { 
      tt <- c("NP", "NN", "RN")
      cols <- cols[cols != 'TrialType']
    }
    else{
      tt <- input$DS_ttbox
    }
    DS_dat <- mean_trans(values$scored_dat, cols, ll=ll, tt=tt)
    cols <- cols[cols != 'Subject']
    ddply(DS_dat, cols, summarize, dv = mean(dv))
  })
  
  output$tTest <- renderTable({
    cols <- c(values$cols, 'TrialType', 'ListLength', 'measure')
    if (input$llbox == 5) { 
      ll <- 5
    }
    else{
      ll <- c(2:8)
      cols <- cols[cols != 'ListLength']
    }
    if (input$ttbox == 1) { 
      tt <- c("NP", "NN", "RN")
      cols <- cols[cols != 'TrialType']
    }
    else{
      if (input$ttbox == 2) { 
        tt <- "NP"
      }
      else{
        if (input$ttbox == 3) { 
          tt <- "NN"
        }
        else{
          tt <- "RN"
        }
      }
    }
    dat_t <- mean_trans(values$scored_dat, cols, ll=ll, tt=tt)
    t_list <- t.test.cog(dat_t, 'Probe.ERR', within=T)
    ldply(t_list$ERR, t.summary)
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
      write.csv(values$dat, file)
      drop_upload(file, dest = "drop_test")
    }
  )

})