

# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

list.of.packages <- c("shiny","devtools","shinydashboard","V8","shinyjs","RColorBrewer", "tidyverse",
                      "leaflet", "leaflet.extras", "sf", "htmltools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]


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
plot <- readRDS("./data/npo_data.rds")
plot$category <- as.factor(plot$category)



#eliminate scientific notation 
options(scipen=999)

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
                                          width = 325, height = "auto",
                                          
                                          ##################check boxes to select types of schools
                                          fluidRow(
                                                  column(12,div(h3("Guatemala Nonprofit Explorer v0.9")))),
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
                                                  column(6, selectInput("sizevar", "Size Variable:", 
                                                                        choices = c("Constant"="constant","Budget" = "budget_adj", "Years Active" = "npo_age"))),
                                                  column(6, selectInput("colorvar", "Color Variable:", 
                                                                        choices = c("Same"="constant","Organization Size"="size",
                                                                                    "Partner Status"="partner_status"))))
                                          )))

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
                        #This part adds the department boundaries, but at this moment in time they are not relevant. 
                        # addPolygons(data=Guatemala_departments,
                        #             stroke = .01,
                        #             smoothFactor = 2,
                        #             fill = F,
                        #             fillOpacity = 1,
                        #             color = "gray",
                        #             weight = 5) %>%
                        addPolygons(data=Guatemala,
                                    stroke = .1,
                                    smoothFactor = 2,
                                    fill = F,
                                    fillOpacity = .1,
                                    color = "black")
        })
        #next we use the observe function to make the drop down dynamic. If you leave this part out you will see that the checkboxes, 
        #when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server 
        #to update the map when the checkboxes are unchecked.
        
        
        
        
        observe({
                
                if(nrow(data())!=0){
                        
                     
                        colorBy <- input$colorvar
                        
                        if (input$colorvar == "constant") {
                                pal <- colorFactor("#4b4b8f", domain=plot[[input$colorvar]])
                        } else if (input$colorvar == "size") {
                                pal <- colorFactor(c("#808080","#7F3C8D", "#11A579", "#3969AC","#F2B701","#E73F74", "#80BA5A"), domain= plot[[input$colorvar]])
                        } else if (input$colorvar == "partner_status") {
                                pal <- colorFactor(c("#7F3C8D" , "#11A579","#3969AC", "#F2B701"), domain= plot[[input$colorvar]])
                        }
                        
                        
                        varname<-switch(input$colorvar,
                                        "constant"="All Nonprofits", 
                                        "size"="Nonprofit Size",
                                        "partner_status"="Pionero Partner Status",
                                        )
                        
                      
                        
                        #some of the functions below break down if data() is empty so I need this if statement
                        #below is how the colors and the legend work, each categorical variable has a sibling that contains color
                        #associated with each value of the variable, e.g. size has sizecolor
                        #below I find the position of the colorvar and the colors are in the column right next to it
                        #I am not using a pallete because when I filter data and the set of possible values changes
                        #the colors would change, e.g. public is green and then blue, that is why I want to "hard code" the colors
                        pos <- match(input$colorvar,names(plot)) # find the column number of the colorvariable
                        colors <- data()[[pos+1]] #this contains vector of colors for each datapoint that will be plotted
                        colors_list <- unique(plot[[pos+1]]) #this gives vector of all possible colors(it uses plot, because I want the legend to be full even if not all values are displayed)
                        values_list <- unique(plot[[pos]]) #this gives vector of all possible values - used for legend
                        
                        # below is how I control size of the marker - 
                        # the size variable has to be "standartized/scaled" because depending on what is chosen as the size variable, these variables could have totally different scale
                        x <- data()[[input$sizevar]] #put the size var in vector x
                        # I take the 95th percentile of the x (size variable) to scale all of the values in x
                        # I took 95th percentile instead of maximum because there are some outlier and they may make the majority of markers tiny
                        # make that proportional to the sqrt of size and multiply by 440 - a constant that makes the size ok (not too big or too small)
                        size <- sqrt(x/quantile(x,0.95,na.rm=TRUE)*440) 
                        
                        leafletProxy("map", data= data()) %>%
                                clearMarkers() %>% #you have to clear previously drawn markers
                                addCircleMarkers(lng=~longitude, lat=~latitude, stroke = FALSE, 
                                                 popup=~paste0("<h3/>", npo,
                                                               "<h5/>", "Parnter Status: ", sep= " ", partner_status,
                                                               "<h5/>", "Nonprofit Size: ", sep= " ", size,
                                                               "<h5/>","Year Founded: ", sep= " ", year_founded,
                                                               "<h5/>","Budget: $", sep=" ", budget,
                                                               "<h5/>","Website: ", sep = " ", website),
                                                 label= ~paste0("Non-Profit: ", sep = " ", npo), radius = size,
                                                 fillOpacity = 0.5, color = "black", fillColor=~pal(plot[[input$colorvar]])) %>%
                                addLegend("bottomleft", pal=pal, values= ~plot[[input$colorvar]], title = varname) 
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