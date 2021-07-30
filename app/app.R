
# setwd("app")
library(shiny)
library(plotly)
library(tidyverse)
source("../R/funs.R")
load("tier1_tables2.Rdata", .GlobalEnv)
# load("tier1_tables2.Rdata.Rdata", .GlobalEnv)
countries <- names(tier1_tables2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WPP - Filling gaps in life table series"),
    helpText(paste0('Work in progress. Last update: ',Sys.Date())),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                'country', 'Tier 1 countries:', choices = countries,
                selectize = FALSE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("plot_ex"),
           br(),
           plotlyOutput("plot_rates"),
           br(),
           plotlyOutput("plot_dispersion"),
           br(),
           plotlyOutput("plot_sex_ratios")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot_rates <- renderPlotly({
        data_country <- tier1_tables2[[input$country]]
        pp <- plot_age_time(data_country$final_data)
        ggplotly(pp)
    })
    
    output$plot_ex <- renderPlotly({
        data_country <- tier1_tables2[[input$country]]
        pp <- plot_ex_time(data_country$final_data)
        ggplotly(pp)
    })
    
    output$plot_dispersion <- renderPlotly({
        data_country <- tier1_tables2[[input$country]]
        pp <- plot_dispersion(data_country$dx_disperison)
        ggplotly(pp)
    })
    
    output$plot_sex_ratios <- renderPlotly({
        data_country <- tier1_tables2[[input$country]]
        pp <- plot_sex_ratios(data_country$sex_ratio)
        ggplotly(pp)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
