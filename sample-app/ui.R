library(shiny)
library(shinythemes)
library(shinyWidgets)
# library(shinydashboard)
library(plotly)
library(DT)
compoundlist <- as.character(c("X-101","X-102","X-103","X-104","X-105","X-106","X-107","X-108","X-109","X-110"))
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme=shinytheme("superhero"),

    # Application title
    titlePanel("Sigmoidal Curve Model Analysis"),

    sidebarLayout(
        sidebarPanel(
            # style = "position:fixed;width:inherit;",
            "Inputs",
            width = 3,
            
            selectInput("compounds","Select Compounds:",
                        choices=compoundlist,
                        selected=as.character(c("X-101","X-103")),
                        multiple=T),
            
            selectInput("control","Select Control Compound:",choices = NULL),
            
            selectInput("batches","Select Batches:",
                        choices=c("A","B"),
                        selected=c("A","B"),
                        multiple=T),
            checkboxGroupButtons("showdata","Select Data to Show:",choices=c("Modeled Data","Raw Data"),
                                 justified = TRUE, status = "primary",
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                 selected = "Modeled Data"),
            checkboxInput("normalize_model", label="Normalize Data to Min/Max", value=TRUE),
            materialSwitch(
                inputId = "showModel",
                label = "Show Model Options", 
                status = "primary",
                right = TRUE
            ),
            
            uiOutput("model_params"),
            
            # checkboxInput("batches_comb", label = "Group by batches", value = F),
            # checkboxInput("batches_comb", label = "Show individual curves", value = F),
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            # style="border-style:solid",
            width=8,
            tabsetPanel(
                tabPanel("Analysis",
                         fluidRow(column(10,
                                         offset=1,
                                         plotlyOutput("modelPlot")
                                         )
                                 ),
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
                                  )
                         ),
                
                tabPanel("Drill Down",
                         fluidRow(column(10,
                                         offset=1),
                                  plotlyOutput("drilldownPlot"),
                                  dataTableOutput("drilldownTable")
                                  )
                         ),
                
                tabPanel("Data")
                # selected="Drill Down"
                )
            )
        )
))
