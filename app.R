



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

list.of.packages <-
        c( "shiny",
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
                "shinyWidgets",
                "rintrojs"
           )

new.packages <-list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]


library(shiny)
library(devtools)
library(shinydashboard)
library(V8)
library(rintrojs)
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

guatemala.shape_orig <- st_read("data/GTM_adm0.shp", stringsAsFactors = FALSE)
Guatemala <-st_transform(guatemala.shape_orig,"+proj=longlat +ellps=WGS84 +datum=WGS84")

guatemala.shape_orig <-st_read("data/GTM_adm1.shp", stringsAsFactors = FALSE)
Guatemala_departments <-st_transform(guatemala.shape_orig,"+proj=longlat +ellps=WGS84 +datum=WGS84")

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ----Header, Siderbar, & Body--- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
#introjsUI(includeOnly = TRUE, cdn = TRUE)

ui <- shinyUI(
        bootstrapPage(
                shinyjs::useShinyjs(),
                introjsUI(),
                tags$head(includeCSS("www/css/bootstrap.css")),
                tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                tags$head(includeScript("www/js/google_analytics.js")),
                tags$head(tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css",
                                    integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")),
                tags$head(tags$script(src = "www/js/wordwrap.js")),
                
                leafletOutput("map", width = "100%", height = "100%"),
                
                absolutePanel(
                        id = "controls",
                        class = "panel panel-default",
                        fixed = TRUE,
                        draggable = FALSE,
                        top = "2.5%",
                        left = "auto",
                        right = "2.5%",
                        bottom = "auto",
                        width = 335,
                        height = "auto",
                        
                        
                        
                        ######################################Sidebar header and nummatching
                        fluidRow(column(6, offset = 1, style='padding:4px;', div(
                                h3("Nonprofit Explorer v1.4"))),
                        
                                column( 4, style='padding:4px;',h6("(",textOutput("num_matching", inline = TRUE),"NPOs selected)",
                                     style = "padding:20px 0 0 0;"))),
                        
                        
                        ######################################Tutorial Button 
                        fluidRow(column(8, offset =1 , style= 'padding:4px;',actionButton(
                                "help", "Tutorial", icon = icon("book-open", class = "fa-pull-left"),
                                style="color: #555555;border-color: #bcbcbc; background: #fff",
                                width = "100%"))),
                        
                        
                        ##################drop down menu to select nonprofit categories
                        fluidRow(column(8,offset = 1, style='padding:4px;',
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
                                
                                column( 2, style='padding:4px;',checkboxInput("select_na", label = "Include NAs?", TRUE))),
                        
                        
                        
                        #######################################graph controls
                        tags$hr(),
                        fluidRow(column(5, offset = 1,style='padding:4px;',selectInput("sizevar","Size Variable:",
                                        choices = c(
                                                "Annual Budget" = "budget_adj",
                                                "Same Size" = "constant",
                                                "Years Active" = "npo_age"),
                                        selected = "constant")),
                                 
                                column(5, style='padding:4px;',selectInput("colorvar","Color Variable:",
                                                choices = c(
                                                        "Nonprofit Size" = "size",
                                                        "Partner Status" = "partner_status",
                                                        "Same Color" ="constant"),
                                                selected = "constant"))),
                        
                        #######################################Search bar
                        fluidRow(column( 10,offset = 1, style='padding:4px;',selectizeInput(
                                        "search",
                                        label = "Search Name: ",
                                        choices = plot$npo,
                                        selected = NULL,
                                        multiple = FALSE,
                                        options = list(
                                                placeholder = 'Select a nonprofit by name',
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
        
        # start introjs when button is pressed with custom options and events
        observeEvent(input$help,
        introjs(session, options = list(
                steps = data.frame(element = c("#category + .selectize-control",
                                               "#select_na",
                                               "#sizevar + .selectize-control",
                                               "#colorvar + .selectize-control",
                                               "#search + .form-control",
                                               "#histBudget "
                                               
                               
                ),
                intro = c(includeMarkdown("tooltips/categories.md"), #This section is used in the tutorial section 
                          includeMarkdown("tooltips/select_na.md"),
                          includeMarkdown("tooltips/sizevar.md"),
                          includeMarkdown("tooltips/colorvar.md"),
                          includeMarkdown("tooltips/search.md"),
                          includeMarkdown("tooltips/hist.md")
                ),
                position = c("auto",
                             "auto",
                             "auto",
                             "auto",
                             "auto",
                             "auto"
                )
                ),
                "nextLabel"="Next",
                "prevLabel"="Previous",
                "skipLabel"="Exit"),
        )
        )


        data <- reactive({
                if (input$search == "") {
                        plot %>% 
                                filter(category %in% input$category) %>% 
                                { 
                                        if(input$select_na) { 
                                                filter(., is.na(budget) | budget, is.na(year_founded) | year_founded)
                                        } else { 
                                                filter(., !is.na(budget), !is.na(year_founded))
                                        }
                                }
                }
                else {
                        filter(plot, npo %in% input$search)
                }
        })
        
        #  setView(lng = -89.506882,lat = 15.883471,zoom = 8) %>%
        
        #create empty map
        output$map <- renderLeaflet({leaflet(plot) %>%
                        fitBounds( -90.74078, 13.52793, -88.0067, 17.78793) %>%
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
                        main = "Nonprofit Partner Budget Distribution",
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
                                                "<h5/>","Annual Budget: $",sep = " ",budget,
                                                "<h5/>","Website: ",sep = " ",website,
                                                "<h5/>","All Categories: ",sep = " ",list_categories),
                                                
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