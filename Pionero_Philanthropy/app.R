#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(devtools)
library(shinydashboard)
library(V8)
library(shinyjs)
library(rintrojs)
library(highcharter)
library(RColorBrewer)
library(htmlwidgets)
library(dplyr)
library(stringr)
library(magrittr)
library(viridis)
library(viridisLite)
library(readr)
library(tidyr)
library(leaflet)
library(treemap)
library(leaflet.extras)
library(sf)

# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----Reference Data & Styles---- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #




# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ----Header, Siderbar, & Body--- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
ui <- fluidPage(

    # Application title
    titlePanel("Guatemala Nonprofit Explorer"),

    # Sidebar with a slider input for number of bins 
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",
                      
                      h2("NonProfit Category"),
                      
                      selectInput("color", "Category", vars),
                      selectInput("size", "Size", vars, selected = "adultpop"),
                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                       # Only prompt for threshold when coloring or sizing by superzip
                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                      ),
                      
                      plotOutput("histCentile", height = 200),
                      plotOutput("scatterCollegeIncome", height = 250)
        ),
    
    
)

# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
