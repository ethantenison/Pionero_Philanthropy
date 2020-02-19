



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

list.of.packages <-
        c(
                "shiny",
                "devtools",
                "shinydashboard",
                "V8",
                "shinyjs",
                "RColorBrewer",
                "tidyverse",
                "leaflet",
                "leaflet.extras",
                "sf",
                "htmltools",
                "shinyWidgets"
        )

new.packages <-
        list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]


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
library(shinyWidgets)


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
options(scipen = 999)

guatemala.shape_orig <- st_read("Guatemala_shape_files/GTM_adm0.shp", stringsAsFactors = FALSE)
Guatemala <-st_transform(guatemala.shape_orig,"+proj=longlat +ellps=WGS84 +datum=WGS84")

guatemala.shape_orig <-st_read("Guatemala_shape_files/GTM_adm1.shp", stringsAsFactors = FALSE)
Guatemala_departments <-st_transform(guatemala.shape_orig,"+proj=longlat +ellps=WGS84 +datum=WGS84")

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ----Header, Siderbar, & Body--- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

ui <- shinyUI(
        bootstrapPage(
                theme = "bootstrap.css",
                shinyjs::useShinyjs(),
                tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                tags$head(includeScript("google_analytics.js")),
                
                leafletOutput("map", width = "100%", height = "100%"),
                
                absolutePanel(
                        id = "controls",
                        class = "panel panel-default",
                        fixed = TRUE,
                        draggable = FALSE,
                        top = 50,
                        left = "auto",
                        right = 20,
                        bottom = "auto",
                        width = 440,
                        height = "auto",
                        
                        ##################drop down menu to select nonprofit categories
                        fluidRow(column(8, div(
                                h3("Nonprofit Explorer v1.4"))),
                        
                                column( 4,h6("(",textOutput("num_matching", inline = TRUE),"NPOs selected)",
                                     style = "padding:20px 0 0 0;"))),
                        
                        
                        fluidRow(column(8,
                                selectInput("category",label = "Select Category",choices =
                                                c(
                                                        "Health",
                                                        "Education",
                                                        "Community Development",
                                                        "Youth & Children",
                                                        "Women & Girls",
                                                        "Human Rights" ,
                                                        "Environment & Conservation",
                                                        "Animal Welfare",
                                                        "Crime",
                                                        "Faith Based",
                                                        "All Nonprofits"
                                                ),selected = c("All Nonprofits"),width = "220px")),
                                
                                column( 1, checkboxInput("select_na", label = "Include NAs?", TRUE))),
                        
                        
                        
                        #######################################graph controls
                        tags$hr(),
                        fluidRow(column(6, selectInput("sizevar","Size Variable:",
                                        choices = c(
                                                "Annual Budget" = "budget_adj",
                                                "Same Size" = "constant",
                                                "Years Active" = "npo_age"))),
                                 
                                column(6, selectInput("colorvar","Color Variable:",
                                                choices = c(
                                                        "Organization Size" = "size",
                                                        "Partner Status" = "partner_status",
                                                        "Same Color" =
                                                                "constant")))),
                        
                        #######################################Search bar
                        fluidRow(column( 12,selectizeInput(
                                        "search",
                                        label = "Search Name: ",
                                        choices = plot$npo,
                                        selected = NULL,
                                        multiple = FALSE,
                                        options = list(
                                                placeholder = 'Select a non profit by name',
                                                onInitialize = I('function() { this.setValue(""); }'))))),
                        ####################################### Histogram of Budget
                        plotOutput("histBudget", height = 200))))

# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #


# create dataframe 'data' to be plotted starts with dataframe 'plot', filtering depends on inputs from the UI
server <- shinyServer(function(input, output, session) {
        
         category <- reactive({
                 category <- plot
        
        
                 category <- filter(category, category %in% input$category)
        
        
                 if(input$select_na){category <- filter(data, is.na(budget) | budget, is.na(year_founded) | year_founded)}
                 else{(category <- filter(category, !is.na(budget), !is.na(year_founded)))}
        
         })
        
         data <- reactive({
                 if(input$search == 'Select a non profit by name') {
                         category()
                 }
                 else {
                         data <- filter(plot, npo %in% input$search)
                 }
         })
        
        
        
        #create empty map
        output$map <- renderLeaflet({leaflet(data) %>%
                        setView(lng = -90.506882,lat = 15.583471,zoom = 8) %>%
                        addTiles() %>%
                        addPolygons(
                                data = Guatemala,
                                stroke = .1,
                                smoothFactor = 2,
                                fill = T,
                                fillOpacity = .05,
                                color = "black")
                        })
        
        #Histogram settings
        his <- plot[!is.na(plot$budget), ]
        BudgetBreaks <-hist(plot = FALSE, his$budget, breaks = 5)$breaks
        output$histBudget <- renderPlot({
                # If no zipcodes are in view, don't plot
                if (nrow(data()) == 0)
                        return(NULL)
                hist(
                        his$budget,
                        breaks = BudgetBreaks,
                        main = "Nonprofit Budget Distribution",
                        xlab = "Annual Budget",
                        xlim = range(his$budget),
                        ylab = "Number of Nonprofits",
                        col = '#A7E0AC',
                        border = 'white'
                    )
                 })
        
        #next we use the observe function to update map when the check box is checked 
        observe({if (nrow(data()) != 0) {
                        colorBy <- input$colorvar
                        sizeBy <- input$sizevar
                        colorData <- data()[[colorBy]]
                        pal <- colorFactor("viridis", colorData)
                        
                        varname <- switch(
                                input$colorvar,
                                "constant" = "All Nonprofits",
                                "size" = "Nonprofit Size",
                                "partner_status" = "Pionero Partner Status")
                        
                        
                        x <-data()[[sizeBy]] 
                        size <-sqrt(x / quantile(x, 0.95, na.rm = TRUE) * 150)
                        
                        
                        leafletProxy("map", data = data()) %>%
                                clearMarkers() %>% #you have to clear previously drawn markers
                                addCircleMarkers(lng =  ~ longitude,lat =  ~ latitude,stroke = FALSE,popup =  ~ paste0(
                                                "<h3/>",npo,"<h5/>","Parnter Status: ",sep = " ",partner_status,
                                                "<h5/>","Nonprofit Size: ",sep = " ",size,
                                                "<h5/>","Year Founded: ",sep = " ",year_founded,
                                                "<h5/>","Budget: $",sep = " ",budget,
                                                "<h5/>","Website: ",sep = " ",website),
                                                
                                                label = ~ paste0("Nonprofit: ", sep = " ", npo),
                                                radius = size,
                                                fillOpacity = 0.75,
                                                color = "black",
                                                fillColor =  ~ pal(colorData)
                                ) %>%
                                clearControls() %>% addLegend("bottomleft",pal = pal, values = colorData, title = varname
                                )
                }
                else{leafletProxy("map") %>% clearMarkers()} #clear the map if the data() is empty
        })
        
        output$num_matching <-renderText({format(nrow(data()), big.mark = ",")})
        
        
})
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

shinyApp(ui, server)