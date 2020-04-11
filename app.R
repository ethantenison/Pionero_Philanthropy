



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
           "rintrojs", 
           "ggplot2"
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
library(ggplot2)



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----Reference Data & Styles---- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #


plot <- readRDS("./data/npo_data.rds")
plot$category <- as.factor(plot$category)
options(scipen = 999)

guatemala.shape_orig <- st_read("data/GTM_adm0.shp", stringsAsFactors = FALSE)
Guatemala <-st_transform(guatemala.shape_orig,"+proj=longlat +ellps=WGS84 +datum=WGS84")

guatemala.shape_orig2 <- st_read("data/GTM_adm1.shp", stringsAsFactors = FALSE)
Guatemala_department <-st_transform(guatemala.shape_orig2,"+proj=longlat +ellps=WGS84 +datum=WGS84")

demographic_map <- readRDS("./data/demographic_map.rds")
demographic_map <- as.data.frame(demographic_map)
demographic_map <- st_as_sf(demographic_map)
demographic_map <- st_transform(demographic_map,"+proj=longlat +ellps=WGS84 +datum=WGS84")
demographic_map <- mutate(demographic_map, formatted = as.character(format(value,  big.mark=",", digits=0)))



# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #


ui <- shinyUI(
        bootstrapPage(
            HTML('<meta name="viewport" content="width=1024">'),
                shinyjs::useShinyjs(),
                introjsUI(),
                tags$head(includeCSS("www/css/bootstrap.css"),
                          includeScript("www/js/google_analytics.js"),
                          tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css",
                                    integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous"),
                          tags$script(src = "www/js/wordwrap.js"),
                          tags$style(".checkbox-inline {margin: 0 !important;}"),
                          tags$style(".selectize-dropdown {top: -200px !important;}"),
                          tags$style(".selectize-input {background: #474949; border-color: #474949; color: white;}"),
                          tags$style(HTML(".selectize-dropdown-content .option {
                              color: black;
                          }
                          
                          ::-webkit-input-placeholder { /* Chrome/Opera/Safari */
                                  color: white;
                          }
                          ::-moz-placeholder { /* Firefox 19+ */
                                  color: white;
                          }
                          :-ms-input-placeholder { /* IE 10+ */
                                  color: white;
                          }
                          :-moz-placeholder { /* Firefox 18- */
                                  color: white;
                          }"))
                          ),
                tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

                tags$style(".pretty.p-default input:dropdown~.state label:after {background-color: #A7E0AC !important;}"), #change color checkbox widgets 
            
        
            

                
                leafletOutput("map", width = "100%", height = "85%"),
                
                absolutePanel(
                        id = "topbar",
                        fixed = TRUE,
                        draggable = FALSE,
                        top = "0%",
                        left = "0%",
                        right = "0%",
                        bottom = "92.5%",
                        
                        
                        fluidRow(
                                column(8, offset = 1, 
                                h1(strong("Guatemala Nonprofit Environment Explorer"),
                                h3("(",textOutput("num_matching", inline = TRUE),"results)"))
                        )
                        
                        
                        )),
            
                 
                     absolutePanel(
                         id = "layercontrols",
                         #class = "panel panel-default",
                         fixed = TRUE,
                         draggable = FALSE,
                         top = "2.5%",
                         left = "90%",
                         right = "2.5%",
                         bottom = "85%",
                         
                         div(id = "layers",
                         fluidRow(
                                 column(3, style='padding-left:20px;',
                                        materialSwitch("non", label= strong("Nonprofit Data"), status= "default",value = TRUE),
                                        materialSwitch("dem", label = strong("Demographic Data"), status = "default" ))
                         ))
                         
                 ),
                
                fluidRow(
                         column(2, style='padding-left:20px;padding-right:0px;',
                                br(),
                                    actionBttn("help", label = "Tutorial",
                                           icon = icon("book-open", class = "fa-pull-left"),
                                           style = "gradient",
                                           color = "primary",
                                           size = "md"
                                ),br(),br(),
                                div(id = "partner_help",
                                    pickerInput("partner", label= "Partner Filters",inline=FALSE,multiple = TRUE, 
                                            options = list(
                                                `actions-box` = TRUE, 
                                                size = 10,
                                                `selected-text-format` = "count > 3"
                                            ),
                                            c("Partnered","Eligible","Not Eligible", "Discontinued Partnership", "No Information"),
                                            selected=c("Partnered")))
                                 ),
                         
                         column(2,style='padding-left:20px;padding-right:0px;',
                                div(id = "category_help",
                                pickerInput("category", label= "Category Filters",inline=FALSE,multiple = TRUE, 
                                            options = list(
                                                `actions-box` = TRUE, 
                                                size = 10,
                                                `selected-text-format` = "count > 3"
                                            ),
                                            c("Health","Education", "Community Development","Youth & Children", "Women & Girls",
                                              "Human Rights" ,"Environment & Conservation","Animal Welfare","Crime"),
                                            selected=c("Health",
                                                       "Education",
                                                       "Community Development",
                                                       "Youth & Children",
                                                       "Women & Girls",
                                                       "Human Rights" ,
                                                       "Environment & Conservation",
                                                       "Animal Welfare",
                                                       "Crime"))),
                                div(id = "department_help",
                                pickerInput("depart_filters", label= "Department Filters",inline=FALSE,
                                            multiple = TRUE, 
                                            options = list(
                                                `actions-box` = TRUE, 
                                                size = 10,
                                                `selected-text-format` = "count > 3"
                                            ),choices =
                                                c("Guatemala", "Quetzaltenango", "Huehuetenango", "Retalhuleu",    
                                                  "Petén", "Quiché", "Chimaltenango",  "Sacatepéquez",  
                                                  "Sololá", "Baja Verapaz", "Izabal", "Jutiapa","Totonicapán",
                                                  "Suchitepéquez", "Escuintla", "El Progreso","Alta Verapaz",
                                                  "Santa Rosa","Zacapa", "Jalapa","Chiquimula","San Marcos"),
                                            selected=c("Guatemala", "Quetzaltenango", "Huehuetenango", "Retalhuleu",    
                                                       "Petén", "Quiché", "Chimaltenango",  "Sacatepéquez",  
                                                       "Sololá", "Baja Verapaz", "Izabal", "Jutiapa","Totonicapán",
                                                       "Suchitepéquez", "Escuintla", "El Progreso","Alta Verapaz",
                                                       "Santa Rosa","Zacapa", "Jalapa","Chiquimula","San Marcos")))
                                
                                
                                
                                
                                ),
                         
                         column(2,style='padding-left:20px;padding-right:0px;',
                                div(id = "size_help",
                                pickerInput("sizevar",label = "Size Variable:",
                                            inline=FALSE,multiple = FALSE,
                                            options = list(
                                                `actions-box` = TRUE, 
                                                size = 10),
                                            choices = c(
                                                "Annual Budget" = "budget_adj",
                                                "Nothing Selected" = "constant",
                                                "Years Active" = "npo_age"),
                                            selected = "constant")),
                                
                                div(id = "color_help",
                                pickerInput("colorvar",label = "Color Variable:",
                                            inline=FALSE,multiple = FALSE,
                                            options = list(
                                                `actions-box` = TRUE, 
                                                size = 10),
                                            choices = c(
                                                "Nonprofit Size" = "size",
                                                "Partner Status" = "partner_status",
                                                "Religious Affiliation" = "faith_based",
                                                "Nothing Selected" ="constant"),
                                            selected = "constant"))
                                

                                ),
                         column(2,style='padding-left:20px;padding-right:0px;',
                                div(id = "demographic_help",
                                pickerInput("demographics",label = "Change Demography",
                                            inline=FALSE,multiple = FALSE,
                                            options = list(
                                                `actions-box` = TRUE, 
                                                size = 10),
                                            choices = c(unique(demographic_map$measure), "Nothing Selected" = "same"),
                                            selected = "same")),
                                
                               
                                selectizeInput("search",
                                               label = "Search Name: ",
                                               choices = plot$npo,
                                               selected = NULL,
                                               multiple = FALSE,
                                               options = list(
                                                   placeholder = 'Select a nonprofit',
                                                   onInitialize = I('function() { this.setValue(""); }')))
                                
                                
                                
                         )
                                
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


# create dataframe 'data' to be plotted starts with dataframe 'plot', filtering depends on inputs from the UI
server <- shinyServer(function(input, output, session) {
        
         
        # start introjs when button is pressed with custom options and events
        observeEvent(input$help,
                     introjs(session, options = list(
                             steps = data.frame(element = c("#partner_help ",
                                                            "#category_help ",
                                                            "#department_help ",
                                                            "#size_help ",
                                                            "#color_help ",
                                                            "#demographic_help",
                                                            "#layers",
                                                            "#search + .form-control",
                                                            "#histBudget "
                                                            
                                                            
                             ),#This section is used in the tutorial section
                             intro = c(includeMarkdown("tooltips/partner.md"),
                                       includeMarkdown("tooltips/categories.md"),
                                       includeMarkdown("tooltips/departments.md"),
                                       includeMarkdown("tooltips/sizevar.md"),
                                       includeMarkdown("tooltips/colorvar.md"),
                                       includeMarkdown("tooltips/demographic.md"),
                                       includeMarkdown("tooltips/layers.md"),
                                       includeMarkdown("tooltips/search.md"),
                                       includeMarkdown("tooltips/hist.md")
                             ),
                             position = c("auto",
                                          "auto",
                                          "auto",
                                          "auto",
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
        
        #######################################Data for Circle markers 
        data <- reactive({
                
                if (input$search == "") {
                        plot %>% 
                                filter(category %in% input$category) %>%
                                filter(partner_status %in% input$partner) %>%
                                filter(department %in% input$depart_filters)
                        
                }
                else {
                        filter(plot, npo %in% input$search)
                }
        })
        
        #######################################Data for Departments 
        
        demographic <- reactive({
                demographic_map %>% dplyr::filter(measure == input$demographics)     
        })        
        
        
        pal2 <- reactive({ colorNumeric(
                palette = "viridis",
                domain = demographic()$value)
        }) 
        
        
        output$map <- renderLeaflet({
                leaflet(data = demographic(), 
                        options = leafletOptions(
                            attributionControl=FALSE)) %>%
                        addProviderTiles("OpenMapSurfer") %>%
                setView(-90.352651, 15.8, zoom = 8)
             
             
        })
        
        
        #######################################Observer Function to have layers shown based on checkboxes 
         observe({
         if (input$non == TRUE) {
             leafletProxy("map") %>% showGroup("Nonprofit Data")
         } else {
             leafletProxy("map") %>% hideGroup("Nonprofit Data")
         }
         })
         
         observe({
         if (input$dem == TRUE) {
             leafletProxy("map") %>% showGroup("Demographic Data")
         } else {
             leafletProxy("map") %>% hideGroup("Demographic Data")
         }
         })
        
        
        
        #######################################Observer Function for Circle Markers & Polygons
        observe({if (nrow(data()) != 0) {
                colorBy <- input$colorvar
                sizeBy <- input$sizevar
                colorData <- data()[[colorBy]]
                pal <- colorFactor("viridis", colorData)
                
                varname <- switch(
                        input$colorvar,
                        "constant" = "All Nonprofits",
                        "size" = "Nonprofit Size",
                        "faith_based"= "Faith Based")
                
                
                x <-data()[[sizeBy]] 
                size <-sqrt(x / quantile(x, 0.95, na.rm = TRUE) * 100)
                
                
                leafletProxy("map") %>% 
                        clearShapes() %>%
                        clearControls() %>%
                        addPolygons(data = Guatemala,
                                        stroke = TRUE,
                                        smoothFactor = 1,
                                        weight = 2, 
                                        color = "Black",
                                        fillOpacity = 0) %>% 
                        addPolygons(    data = demographic(),
                                        stroke = TRUE,
                                        smoothFactor = 1,
                                        fillColor = ~ pal2()(demographic()$value),
                                        fillOpacity = .25,
                                        weight = 3, 
                                        color = "black",
                                        highlight = highlightOptions(
                                                weight = 5,
                                                fillOpacity = 0.7,
                                                bringToFront = FALSE),
                                        popup =  ~ paste0(
                                                "<h4/><b>",department,"</b><h5/>","Measure: ",sep = " ",input$demographics,
                                                "<h5/>","Value: ",sep = " ",demographic()$formatted),
                                        group = "Demographic Data") %>%
                        clearMarkers() %>% #you have to clear previously drawn markers
                        addCircleMarkers(data = data(), lng =  ~ longitude,lat =  ~ latitude,stroke = FALSE,popup =  ~ paste0(
                                "<h4/><b>",npo,"</b><h5/>","Partner Status: ",sep = " ",partner_status,
                                "<h5/>","Nonprofit Size: ",sep = " ",size,
                                "<h5/>","Year Founded: ",sep = " ",year_founded,
                                "<h5/>","Annual Budget: $",format(budget, big.mark=","),
                                "<h5/>","Website: ",sep = " ",website,
                                "<h5/>","All Categories: ",sep = " ",list_categories),
                                
                                label = ~ paste0("Nonprofit: ", sep = " ", npo),
                                radius = size,
                                fillOpacity = 0.5,
                                color = "black",
                                fillColor =  ~ pal(colorData),
                                group = "Nonprofit Data"
                        )
                
                #######################################Legends based on which layer checkboxes are ticked
                    if (input$non == TRUE & input$dem == TRUE) {
                        leafletProxy("map") %>% 
                            addLegend(data = demographic(), "bottomright", pal = pal2(), values = ~demographic()$value,
                                      opacity = 0.7, title = ~ paste0(unique(units)), group = "Demographic Data", layerId = "demleg") %>% 
                            addLegend(data = data(), "bottomright",pal = pal, values = colorData, title = varname,
                                      group = "Nonprofit Data", layerId = "nonleg")
                    }
                    else if(input$dem == TRUE){
                        leafletProxy("map") %>% 
                            addLegend(data = demographic(), "bottomright", pal = pal2(), values = ~demographic()$value,
                                      opacity = 0.7, title = ~ paste0(unique(units)), group = "Demographic Data", layerId = "demleg")
                    }
                
                    else if (input$non == TRUE) {
                        leafletProxy("map") %>%
                            addLegend(data = data(), "bottomright",pal = pal, values = colorData, title = varname,
                                      group = "Nonprofit Data", layerId = "nonleg")
                    }
                    
        }
                else{leafletProxy("map") %>% clearMarkers()} #clear the map if the data() is empty
                
                
              
                
                
                
        })
        
        output$num_matching <-renderText({format(nrow(data()), big.mark = ",")})
        
        
        #below is what is needed for the "unfolding" UI
        shinyjs::onclick("togglecategories",
                         shinyjs::toggle(id = "filtercategories", anim = TRUE))
        
        shinyjs::onclick("toggleother",
                         shinyjs::toggle(id = "filterother", anim = TRUE))
        
        shinyjs::onclick("toggledepartment",
                         shinyjs::toggle(id = "filterdepartment", anim = TRUE))
        
        
})
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

shinyApp(ui, server)