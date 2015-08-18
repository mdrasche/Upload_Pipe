library(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("aggregator", 
                         label = h3("Aggregate Over"), 
                         choices = list('List Length' = 'ListLength', 
                                        'Trial Type' = 'TrialType',
                                        'Long vs. Short LL' = 'Block',
                                        'High vs. Low RN' = 'Cond')
                         )
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
               DT::dataTableOutput('mytable')
                #tableOutput('table')
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