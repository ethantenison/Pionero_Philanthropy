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
library(htmltools)




# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----Reference Data & Styles---- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

#It has to be an .rds file so that the accents are maintained 
plot <- readRDS("./data/guatemala_data.rds")
plot$category <- as.factor(plot$category)

guatemala.shape_orig <- st_read("Guatemala_shape_files/GTM_adm0.shp", stringsAsFactors=FALSE)
Guatemala <- st_transform(guatemala.shape_orig, "+proj=longlat +ellps=WGS84 +datum=WGS84")

guatemala.shape_orig <- st_read("Guatemala_shape_files/GTM_adm1.shp", stringsAsFactors=FALSE)
Guatemala_departments <- st_transform(guatemala.shape_orig, "+proj=longlat +ellps=WGS84 +datum=WGS84")



# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ----Header, Siderbar, & Body--- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

ui <- shinyUI(bootstrapPage(theme="bootstrap.css",
                            shinyjs::useShinyjs(),                      
                            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                            tags$head(includeScript("google_analytics.js")),
                            
                            leafletOutput("map", width = "100%", height = "100%"),
                            
                            #this allows me to put the checkmarks ontop of the map to allow people to 
                            #view earthquake depth or overlay a heatmap
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                              width = 225, height = "auto",
                              
                              ##################check boxes to select types of schools
                              fluidRow(
                                      column(12,div(h3("Guatemala Nonprofit Explorer v0.3")))),
                              fluidRow(
                                      
                                      column(10, selectInput("category", label="Select Category", choices = 
                                                                   c("Health", "Education", "Community Development","Youth & Children",          
                                                                     "Women & Girls", "Human Rights" ,"Environment & Conservation",
                                                                     "Animal Welfare","Crime", "All Nonprofits"),
                                                                   selected=c("All Nonprofits")))
                                      ),
                              
                              #######################################graph controls
                              tags$hr(),
                              fluidRow(
                                column(6, selectInput("sizevar", "Size variable:", 
                                                      choices = c("Constant"="constant","Budget" = "budget"))),
                                column(6, selectInput("colorvar", "Color variable:", 
                                                      choices = c("Constant"="constant","Organization Size"="size",
                                                                  "Partner Status"="partner_status", "Year Established"="year"))))
                              
                              
                              )
                )
            )

# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #


server <- shinyServer(function(input, output, session) {
        #define the color pallate for the categories of NPO's 
        
        
        # create dataframe 'data' to be plotted starts with dataframe 'plot', filtering depends on inputs from the UI
        data <- reactive({
                data <- plot
                
                data <- filter(data, category %in% input$category)
                
                
        })
    
        #create empty map
        output$map <- renderLeaflet({
                leaflet(data) %>% 
                        setView(lng = -90, lat = 15, zoom = 7)  %>% #setting the view over ~ center of  Guatemala
                        addTiles() %>%
                        addPolygons(data=Guatemala_departments,
                                    stroke = .01,
                                    smoothFactor = 2,
                                    fill = F,
                                    fillOpacity = .1,
                                    color = "gray") %>%
                        addPolygons(data=Guatemala,
                                    stroke = .1,
                                    smoothFactor = 2,
                                    fill = F,
                                    fillOpacity = .1,
                                    color = "black")
        })
        #next we use the observe function to make the drop down dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
        observe({
                
               
                if(nrow(data())!=0){
                        leafletProxy("map", data= data()) %>%
                                clearMarkers() %>% #you have to clear previously drawn markers
                                addCircleMarkers(lng=~longitude, lat=~latitude, stroke = FALSE, 
                                                 popup=~paste0("<br/>","Non-Profit: ", sep = " ", website), 
                                                 label= ~paste0("Non-Profit: ", sep = " ", name), 
                                                 fillOpacity = 0.5,radius= 10, color = "black", fillColor="purple")  
                }
                else{leafletProxy("map") %>% clearMarkers()} #clear the map if the data() is empty
        })
      
        
        
        
})
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

shinyApp(ui, server)