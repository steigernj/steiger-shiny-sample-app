library(shiny)
source("plots.R")
source("modelling.R")
source("drill_down_plots.R")
source("plot_and_table_utils.R")
shinyServer(function(input, output, session) {
    shinyalert(
        title = "Hello",
        text = "Dose-response based experiments are used throughout the drug discovery process in order to compare novel molecules' efficacy, potency, and mitogenicity performances during cell-based assays. Analysis of this data plays a vital role in research planning and progress. This tool is meant to be an example of dose-response data modeling and analysis using data similar to that resulting from cell-based assays.",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Continue",
        confirmButtonCol = "#E38124",
        timer = 0,
        imageUrl = "",
        animation = TRUE
    )
    
    #update choices from input$control based on selected compounds
    observe({
        x <- input$compounds
        if(is.null(x)){
            x <- character(0)
        }
        updateSelectInput(session,"control","Select Control Compound:",
                          choices=x)
    })
    
    #create and subset df !! fix this, this sucks
    my_df <- reactive({
        df <- read.csv("results.csv")
        df$date <- as.Date(df$date,format="%m/%d/%y")
        df$experiment_id <- paste(df$compound,df$batch,df$date,sep="_")
        df <- subset(df,compound%in%input$compounds & batch%in%input$batches)
        df
    })
    
    # output$modelParams <- renderUI({
    #     if(input$showModel==TRUE) {
    #         selectInput("model_version","Select Function to Use:",choices=c("5-phase Log-Log","4-phase Log-Log","3-phase Log-Log","2-phase Log-Log"),selected="4-phase Log-Log",multiple = FALSE)
    #     }
    # })
    
    output$modelEquation <- renderUI({
        if(is.null(input$model_version)){
        } else if(input$model_version=="5-phase Log-Log"){
            withMathJax(
                helpText('Function:   $$f(x, (b,c,d,e,f)) = c+\\frac{d-c}{f+exp(b(log(x)-log(e)}$$')
            )
        } else if(input$model_version=="4-phase Log-Log"){
            withMathJax(
                helpText('Function:   $$f(x, (b,c,d,e,1)) = c+\\frac{d-c}{1+exp(b(log(x)-log(e)}$$')
            )
        } else if(input$model_version=="3-phase Log-Log"){
            withMathJax(
                helpText('Function:   $$f(x, (b,0,d,e,1)) = \\frac{d}{1+exp(b(log(x)-log(e)}$$')
            )
        } else if(input$model_version=="2-phase Log-Log"){
            withMathJax(
                helpText('Function:   $$f(x, (b,0,1,e,1)) = \\frac{1}{1+exp(b(log(x)-log(e)}$$')
            )
        }
    })
    
    output$modelPlot <- renderPlotly({
        df <- my_df()
        modeled_df <- split_and_model(df=df,model_version=input$model_version)
        p <- model_plot(df=df,model_df=modeled_df,showdata=input$showdata,control_sel=input$control,normalize_input=input$normalize_model,model_version=input$model_version)
        ggplotly(p)
    })
    
    output$barPlot <- renderPlotly({
        df <- my_df()
        modeled_df <- split_and_model(df=df,model_version=input$model_version)
        p <- bar_plot(df=modeled_df,control_sel=input$control)
        ggplotly(p)
    })
    
    output$percPlot <- renderPlotly({
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
        ggplotly(p) %>% layout(height=600)
    })
    
    output$drilldownTable <- renderDataTable({
        df <- my_df()
        drill_down_table(df)
    })
    
    output$summaryTable <- renderDataTable({
        df <- my_df()
        modeled_df <- split_and_model(df)
        build_summary_table(df=modeled_df,control_sel=input$control)
    })
    
    output$rawTable <- renderDataTable({
        df <- my_df()
        build_raw_table(df)
    })
    
    output$modelTable <- renderDataTable({
        df <- my_df()
        modeled_df <- split_and_model(df)
        build_model_table(modeled_df)
    })

})
