#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(shiny.sanitize.errors = FALSE)

library(plotly)
library(shiny)
library(DT)
source("PCA.R")
source("snip.param.R")

param.name <- "NULL"

weather.data.wrnmd.hist <- 
    read.csv("produkt_klima_tag_19470101_20171231_04271.txt",
             sep = ";", strip.white = TRUE,
             na.strings = c("-999"),
             dec = ".") %>%
    mutate(date = as.POSIXct(strptime(MESS_DATUM, format = "%Y%m%d")),
           month = ordered(format(date, "%B"), levels = month.name),
           year = format(date, "%Y"))

raw.data <- weather.data.wrnmd.hist
#not.complete <- which(!complete.cases(raw.data))
#not.numeric <- as.numeric(which(!(sapply(raw.data, inherits, c("numeric", "integer")))))

shinyServer(function(input, output) {
    
    inputdata <- reactive({
        print(paste(Sys.time(), "run server > inputdata", sep = ":    "))
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(raw.data)
        
        read.csv(inFile$datapath,
                 strip.white = TRUE,
                 na.strings = input$na.strings,
                 dec = input$dec,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })
    
    output$raw.tbl <- renderDataTable({
        
        datatable(inputdata(), 
                  #rownames = FALSE, ATTENTION!! this option shifts column numbering of selections!!!
                  filter = "top",
                  options = list(lengthChange = FALSE),
                  selection = list(target = 'column'))
    })
    
    processed <- reactive({
        raw.data <- inputdata()
        raw.cols         <- 1:ncol(raw.data)
        de.selected.cols <- input$raw.tbl_columns_selected
        valid.cols       <- raw.cols[!(raw.cols %in% de.selected.cols)]
        
        filtered.rows <- input$raw.tbl_rows_all
        
        filtered <- raw.data[filtered.rows, valid.cols]
        
        quality.checked <- data.quality(filtered)
        
        measure.vars <-     quality.checked$measure.vars
        data <-             quality.checked$data
        parameters <- names(quality.checked$data)
        
        missings <- 
            data.frame(param.name = names(quality.checked$missings.per.parameter),
                       missing = quality.checked$missings.per.parameter) %>%
            filter(param.name %in% measure.vars) %>%
            arrange(desc(missing)) %>%
            mutate(comp = paste0(param.name, " (", missing, ")"))
        
        list(quality.checked = quality.checked,
             measure.vars = measure.vars,
             data = data,
             parameters = parameters,
             missings = missings,
             filtered.data = filtered)
    })
    
    output$param.barplot <- renderPlot({
        filtered.data <- processed()$filtered.data
        #print(str(filtered.data))
        
        parameter.table <- sapply(names(filtered.data),
                                  function(x) snip.param(filtered.data, x)$balance) %>%
            as.data.frame() %>%
            mutate(balance = rownames(.)) %>%
            gather(variable, value, -balance) %>%
            filter(balance == "net") %>%
            mutate(value = as.numeric(value)) %>%
            arrange(value) %>%
            mutate(variable = ordered(variable, levels = variable),
                   sign = ifelse(value > 0, "plus", "minus"))
        
        ggplot(parameter.table, aes(x = variable, y = value, fill = sign)) +
            geom_col(show.legend = FALSE) +
            coord_flip() +
            xlab("Parameter") +
            ylab("Net loss/gain of data values\n for PCA analysis on \nremoval of a specific parameter") +
            theme_dark(base_size = 15)
    })
    
    
    ## PCA part:
    
    
    output$secondSelection <- renderUI({
        checkboxGroupInput(inputId = "measureVars",
                           label = "Select/deselect the numeric parameters to be considered for the PCA (missing values in parentheses)",
                           #choices = parameters,
                           choiceNames =  processed()$missings$comp,
                           choiceValues = processed()$missings$param,
                           selected =     processed()$missings$param,
                           inline = TRUE)
    })
    
    output$thirdSelection <- renderUI({
        selectInput(inputId = "group.fill",
                    label = "Grouping parameter",
                    choices = processed()$parameters,
                    multiple = FALSE)
    })
    
    pca.results <- reactive({
        data <- processed()$data
        meas.v <- input$measureVars
         #if (is.null(meas.v)) meas.v <- 0
        cat("\tCurrent measure vars:\t", meas.v, "\n")
        try(pca(.data = data,
            .measure.vars = meas.v), silent = TRUE)
    })
    
    output$pca.tbl <- renderTable({
        pca.results()$scores
    })
    
    output$PCA.plotly <- renderPlotly({
        
        ggplotly(pca.plot(loadings = pca.results()$loadings,
                          scores = pca.results()$scores,
                          fill.group = input$group.fill,
                          arrow.scale = input$arrow.scale,
                          pointsize = input$pointsize))
        
    })
    
    output$PCA.plot <- renderPlot({
        
        try({pca.plot(loadings = pca.results()$loadings,
                 scores = pca.results()$scores,
                 fill.group = input$group.fill,
                 arrow.scale = input$arrow.scale,
                 pointsize = input$pointsize)}, silent = TRUE)
    })
    
    output$loadings.plot <- renderPlot({
        loadings.plot(loadings = pca.results()$loadings)
    }, width = 600, height = 300)
    
    output$proportions.plot <- renderPlot({
        proportions.plot(proportions = pca.results()$proportions)
    }, width = 600, height = 300)
    
})
