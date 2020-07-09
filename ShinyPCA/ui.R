#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(plotly)
library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Dynamic Principal Component Analysis"),
    
    tabsetPanel(
        tabPanel("Data selection from raw data table", 
                 sidebarLayout(
                     sidebarPanel(
                         p('Principal Component Analysis requires a data set without missing values. Therefore, all rows containing missing values are removed from the input data before the analysis.'), 
                         p('Removing a whole data column can prevent a lot of values in the other columns from being removed like this. The barplot below shows the net gain or loss of data values if one column is removed.'),
                         p('Deselect columns by clicking on the table to the right.'),
                         p('You can also use the filter fields to analyze only a subset of the data.'),
                         plotOutput('param.barplot')
                     ),
                     mainPanel(
                         tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
                         div(dataTableOutput('raw.tbl'), style = "font-size:70%")
                     )
                 )
        ),

        tabPanel("PCA analysis",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("secondSelection"),
                         uiOutput("thirdSelection"),
                         sliderInput(inputId = "arrow.scale",
                                     label = "Arrow length",
                                     min = 0, max = 20, value = 5,
                                     step = .1),
                         
                         sliderInput(inputId = "pointsize",
                                     label = "Point size",
                                     min = 0, max = 10, value = 2,
                                     step = .5)
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("PCA plot", 
                                      #actionButton("goButton", "Go!"),
                                      plotOutput("PCA.plot", height = "800px")
                                      ), 
                             tabPanel("Proportions/Loadings",
                                      plotOutput("proportions.plot"),
                                      plotOutput("loadings.plot")),
                             tabPanel("analyse PCA plot (experimental, be patient!)",
                                      plotlyOutput("PCA.plotly", height = "800px")
                                      )
                         )
                     )
                 )
        ),
        tabPanel("PCA Scores (may take some time with large tables)", tableOutput('pca.tbl')),
        tabPanel("Analyse your own data!",
                 fluidRow(
                     p(),
                     p('Pre-loaded is an example data set from the German Weather Service (DWD ',
                     a(href = "German Weather Service (DWD)", 'https://www.dwd.de/DE/leistungen/klimadatendeutschland/klarchivtagmonat.html'),
                     ') containing observations of cloud coverage, air pressure, rainfall, form of precipitation, sunshine duration, air temperature, relative humidity and many others for the station Warnemünde (northern Germany) over the period of 1947 - 2017.'),
                     p('You can upload own data sets here. Data must be in text format (txt, csv, ...). First set the import parameters. Be sure to set a correct NA string, otherwise individual columns may not be recognized as numeric. Then use the BROWSE button to select a file from your hard drive.'),
                     p(),
                     column(3,
                            radioButtons('quote', 'Quote',
                                         c("None" = '',
                                           'Double Quote' = '"',
                                           'Single Quote' = "'"),
                                         '"')),
                     
                     column(3,
                            radioButtons(inputId = 'sep',
                                         label = 'Separator',
                                         choices = c(Comma = ',',
                                                     Semicolon = ';',
                                                     Tab = '\t'),
                                         selected = ',')),
                     
                     column(3,
                            radioButtons(inputId = 'dec', 
                                         label = 'Decimal',
                                         choices = c(Comma = ',',
                                                     Colon = '.'),
                                         selected = '.')),
                     
                     column(3,
                            checkboxInput('header', 'Header', TRUE),
                            selectizeInput("na.strings", "NA String",
                                      choices = c("", "NA", "na", "N/A", "<1"),
                                      selected = "",
                                      options = list(create = TRUE),
                                      multiple = TRUE))),
                 
                 fluidRow(fileInput('file1', 'Choose file to upload',
                                    accept = c(
                                        'text/csv',
                                        'text/comma-separated-values',
                                        'text/tab-separated-values',
                                        'text/plain',
                                        '.csv',
                                        '.tsv'))),
                 p('If you want a sample .csv or .tsv file to upload,',
                   'you can first download the sample',
                   a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
                   a(href = 'pressure.tsv', 'pressure.tsv'),
                   'files, and then try uploading them.'
                 ))
    )
))