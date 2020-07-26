library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
# library(shinydashboard)
library(plotly)
library(DT)

compoundlist <- as.character(c("X-101","X-102","X-103","X-104","X-105","X-106","X-107","X-108","X-109","X-110"))
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme=shinytheme("united"),
                  useShinyalert(),
    titlePanel("Sigmoidal Curve Model Analysis"),

    sidebarLayout(
        sidebarPanel(
            # style = "position:fixed;width:inherit;",
            "Inputs",
            width = 3,
            
            selectInput("compounds","Select Compounds:",
                        choices=compoundlist,
                        selected=as.character(c("X-101","X-102","X-103")),
                        multiple=T),
            
            selectInput("control","Select Control Compound:",choices = NULL),
            
            selectInput("batches","Select Compound Batches:",
                        choices=c("A","B"),
                        selected=c("A","B"),
                        multiple=T),
            checkboxGroupButtons("showdata","Select Data to Show:",choices=c("Modeled Data","Raw Data"),
                                 justified = TRUE, status = "primary",
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                 selected = "Modeled Data"),
            checkboxInput("normalize_model", label="Normalize Data to Response Min/Max", value=TRUE),
            # materialSwitch(
            #     inputId = "showModel",
            #     label = "Show Model Options", 
            #     status = "primary",
            #     right = TRUE
            # ),
            # uiOutput("modelParams"),
            
            selectInput("model_version","Select Function to Use:",choices=c("5-phase Log-Log","4-phase Log-Log","3-phase Log-Log","2-phase Log-Log"),selected="4-phase Log-Log",multiple = FALSE),
            uiOutput("modelEquation"),
            tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4696819/", "Source"),
            h6("*More options under construction")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            # style="border-style:solid",
            width=8,
            tabsetPanel(
                tabPanel("Model Analysis",
                         h2(tags$u("Model Analysis")),
                         fluidRow(column(10,
                                         offset=1,
                                         plotlyOutput("modelPlot")
                                         )
                                 ),
                         tags$br(),
                         fluidRow(column(5,
                                         offset=1,
                                         plotlyOutput("barPlot")
                                         ),
                                  column(5,
                                         # offset=1,
                                         plotlyOutput("percPlot")
                                         )
                                  ),
                         fluidRow(column(10,
                                         offset=1,
                                         dataTableOutput("resultsTable")
                                         )
                                  ),
                         fluidRow(
                             h4("*Bar plot error bars are currently broken.")
                         )
                         ),
                
                tabPanel("Statistical Analysis",
                         fluidRow(column(10,
                                         offset=1),
                                  # style='height:40vh',
                                  h2(tags$u("Under Construction")),
                         )
                ),
                
                tabPanel("Drill Down",
                         fluidRow(column(10,
                                         offset=1),
                                  style='height:70vh',
                                  h2(tags$u("Curve Models - Individual Experiments")),
                                  plotlyOutput("drilldownPlot"),
                                  ),
                         fluidRow(column(10,
                                         offset=1),
                                  h2(tags$u("Selected Study Details:")),
                                  dataTableOutput("drilldownTable")
                                  )
                         ),
                
                tabPanel("Data",
                         fluidRow(column(10,
                                         offset=1),
                                  h2(tags$u("Relevant Data and Summary Tables")),
                                  tags$br(),
                                  h3("Summary:"),
                                  dataTableOutput("summaryTable"),
                                  tags$br(),
                                  h3("Raw Data:"),
                                  dataTableOutput("rawTable"),
                                  tags$br(),
                                  h3("Modeled Data"),
                                  dataTableOutput("modelTable"),
                                  )
                         )
                # selected="Data"
                )
            )
        )
))