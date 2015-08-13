library(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("llbox", 
                   label = h3("List Lengths Included"), 
                   choices = list("Aggregate" = 1, 
                                  "List Length 5 Only" = 5),
                   selected = 5),
      radioButtons("ttbox", 
                   label = h3("Trial Types Included"), 
                   choices = list("Aggregate" = 1, 
                                  "NP Only" = "NP",
                                  "NN Only" = "NN",
                                  "RN Only" = "RN"),
                   selected = "NP")
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Uploading Files",
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
               actionButton("clean", "Clean Data"),
               downloadLink('downloadData', 'Download cleaned up data'),
               p('Click',
                 a(href = '', 'here'), 'for details on what to upload.'
               )
      ),
      tabPanel("Descriptive Statistics",
               #TODO Choose which statistics to include--checkbox
                tableOutput('table')
               ),
      tabPanel("t-tests",
              #TODO Choose what comparisons to make
               tableOutput("tTest")
               ),
      tabPanel("Outlier Analysis",
                plotOutput("outlier_plot"),
                plotOutput("outlier_plot_tt")
      )
    )
  )
)))