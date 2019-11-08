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
library(RColorBrewer)
library(dplyr)
library(readr)
library(tidyr)
library(leaflet)
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

plot <- readRDS(file="./data/guatemala_data.rds")


# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ----Header, Siderbar, & Body--- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                  width = 125, height = "auto",
                  
                  ##################check boxes to select types of schools
                  fluidRow(
                      column(7,div(style="display:inline-block",h3("Guatemala Nonprofit Explorer v0.1")))),
                  fluidRow(
                      
                      column(5, checkboxGroupInput("category", label=NULL,
                                                   c("Health", "Education", "Community Development","Youth & Children",          
                                                     "Women & Girls", "Human Rights" ,"Environment & Conservation",
                                                     "Animal Welfare","Crime"),
                                                   selected=c("Health", "Education", "Community Development","Youth & Children",          
                                                              "Women & Girls", "Human Rights" ,"Environment & Conservation",
                                                              "Animal Welfare","Crime")))
                      
                  )
   
))
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
server <- function(input, output, session) {
    data <- reactive({
        data <- plot
        
        data <- filter(data, category %in% input$category)
       
        
    })
    
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>%
            setView(lng = -90.40, lat = 15.47, zoom = 7)
    })

     observe({if(nrow(data())!=0){   #some of the functions below break down if data() is empty so I need this if statement
         #below is how the colors and the legend work, each categorical variable has a sibling that contains color
         #associated with each value of the variable, e.g. type has typecolor
         #below I find the position of the colorvar and the colors are in the column right next to it
         #I am not using a pallete because when I filter data and the set of possible values changes
         #the colors would change, e.g. public is green and then blue, that is why I want to "hard code" the colors
         pos <- match(input$category,names(plot)) # find the column number of the colorvariable
         colors <- data()[[pos+1]] #this contains vector of colors for each datapoint that will be plotted
         colors_list <- unique(plot[[pos+1]]) #this gives vector of all possible colors(it uses plot, because I want the legend to be full even if not all values are displayed)
         values_list <- unique(plot[[pos]]) #this gives vector of all possible values - used for legend
         # below is how I control size of the marker -
         # the size variable has to be "standartized/scaled" because depending on what is chosen as the size variable, these variables could have totally different scale
         #x <- data()[[input$sizevar]] #put the size var in vector x
         # I take the 95th percentile of the x (size variable) to scale all of the values in x
         # I took 95th percentile instead of maximum because there are some outlier and they may make the majority of markers tiny
         # make that proportional to the sqrt of size and multiply by 440 - a constant that makes the size ok (not too big or too small)
         #size <- sqrt(x/quantile(x,0.95,na.rm=TRUE)*440)
         leafletProxy("map", data= data()) %>%
             clearMarkers() %>% #you have to clear previously drawn markers
             addCircleMarkers(lng=~longitude, lat=~latitude, stroke = FALSE,
                             label= ~name, fillOpacity = 0.5,
                              fillColor = colors) %>%
            addLegend("bottomleft",  colors=colors_list, labels=values_list, layerId="legend")
     }
         else{leafletProxy("map") %>% clearMarkers()} #clear the map if the data() is empty
     })
    
     output$num_matching <- renderText({format(nrow(data()),big.mark = ",")})
    
}

#radius= size, popup=~popup, 
# Run the application 
shinyApp(ui = ui, server = server)
