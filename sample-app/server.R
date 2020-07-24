library(shiny)
source("plots.R")
source("modelling.R")
source("drill_down_plots.R")
shinyServer(function(input, output, session) {
    
    observe({
        x <- input$compounds
        if(is.null(x)){
            x <- character(0)
        }
        updateSelectInput(session,"control","Select Control Compound:",
                          choices=x)
    })
    
    my_df <- reactive({
        df <- read.csv("results.csv")
        df$date <- as.Date(df$date,format="%m/%d/%y")
        df$experiment_id <- paste(df$compound,df$batch,df$date,sep="_")
        df <- subset(df,compound%in%input$compounds & batch%in%input$batches)
        df
        # return(df)
    })
    my_df_model <- reactive({
        
    })
    
    output$model_params <- renderUI({
        if(input$showModel==TRUE) {
            selectInput("model_version","Select Function to Use:",choices=c("4-phase Log-Log","3-phase Log-Log"),selected="4-phase Log-Log",multiple = FALSE)
        }
    })
    
    output$modelPlot <- renderPlotly({
        # df <- read.csv("results.csv")
        # df$date <- as.Date(df$date,format="%m/%d/%y")
        df <- my_df()
        modeled_df <- split_and_model(df=df,model_version=input$model_version)
        p <- raw_plot(df=df,model_df=modeled_df,showdata=input$showdata,control_sel=input$control,normalize_input=input$normalize_model)
        # ggplotly(p)
    })
    
    output$barPlot <- renderPlotly({
        # df <- read.csv("results.csv")
        # df$date <- as.Date(df$date,format="%m/%d/%y")
        # df_ <- subset(df,compound%in%input$compounds & batch%in%input$batches)
        df <- my_df()
        modeled_df <- split_and_model(df=df,model_version=input$model_version)
        p <- bar_plot(df=modeled_df,control_sel=input$control)
        ggplotly(p)
    })
    
    output$percPlot <- renderPlotly({
        # df <- read.csv("results.csv")
        # df$date <- as.Date(df$date,format="%m/%d/%y")
        # df_ <- subset(df,compound%in%input$compounds & batch%in%input$batches)
        df <- my_df()
        modeled_df <- split_and_model(df=df,model_version=input$model_version)
        p <- perc_plot(df=modeled_df,control_sel=input$control)
        ggplotly(p)
    })
    
    output$resultsTable <- renderDataTable({
        
    })
    
    output$drilldownPlot <- renderPlotly({
        df <- my_df()
        modeled_df <- split_and_model(df)
        p <- drill_down_model_plot(modeled_df)
        ggplotly(p)
    })
    
    output$drilldownTable <- renderDataTable({
        df <- my_df()
        drill_down_table(df)
    })

})
