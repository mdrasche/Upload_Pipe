library(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   '\t'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      tags$hr(),
      p('Click',
        a(href = '', 'here'), 'for details on what to upload.'
      )
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Descriptive Statistics",
                tableOutput('table'),
               radioButtons("DS_llbox", 
                            label = h3("List Lengths Included"), 
                            choices = list("Aggregate" = 1, 
                                           "List Length 5 Only" = 5),
                            selected = 5),
               radioButtons("DS_ttbox", 
                            label = h3("Trial Types Included"), 
                            choices = list("Aggregate" = 1, 
                                           "NP Only" = "NP",
                                           "NN Only" = "NN",
                                           "RN Only" = "RN"),
                            selected = "NP"),
                downloadLink('downloadData', 'Download')),
      tabPanel("t-tests",
               radioButtons("llbox", 
                                  label = h3("List Lengths Included"), 
                                  choices = list("Aggregate" = 1, 
                                                 "List Length 5 Only" = 5),
                                  selected = 5),
               radioButtons("ttbox", 
                            label = h3("Trial Types Included"), 
                            choices = list("Aggregate" = 1, 
                                           "NP Only" = 2,
                                           "NN Only" = 3,
                                           "RN Only" = 4),
                            selected = 2),
               tableOutput("tTest")
               ),
      tabPanel("Outlier Analysis",
                plotOutput("outlier_plot"),
                plotOutput("outlier_plot_tt")
      )
    )
  )
)))