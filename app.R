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
library(extrafont)



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----Reference Data & Styles---- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

source("./data/SwitchButton.R")

plot <- readRDS("./data/npo_data.rds")
plot$category <- as.factor(plot$category)
names <- dplyr::select(plot, npo)
names[nrow(names) + 1,] = c("None Selected")
names <- unique(names$npo)

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
demographic_map <- mutate(demographic_map, Population = as.character(format(Population,  big.mark=",", digits=0)))


definitions <- htmlTemplate("tooltips/definitions.html")



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
                theme = "www/css/button.css",
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
                          tags$style(".selectize-input {background: #193A45 ; border-color: #193A45 ; color: #F2F2F2;}"),
                          tags$style('h2 {color:#193A45;}'),
                          tags$style('h4 {color:#193A45;}'),
                          tags$style(HTML(".selectize-dropdown-content .option {
                              color: #193A45;
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
                
                tags$style(".pretty.p-default input:dropdown~.state label:after {background-color: #486F73 !important;}"), #change color checkbox widgets, also change css
    
                leafletOutput("map", width = "100%", height = "91.5%"),
                
                absolutePanel(
                        id = "topbar",
                        fixed = TRUE,
                        draggable = FALSE,
                        top = "0%",
                        left = "0%",
                        right = "80%",
                        bottom = "95%",
                        
                        
                        fluidRow(
                                column(1,style='padding-right:0px;width:150px;padding-top:10px;', offset = 1, 
                                       img(src="images/logo.png", height  = 125, width = 125),
                                          h4(strong("(",textOutput("num_matching", inline = TRUE)," nonprofits)"))),
                                column(1, style='padding-left:0px;padding-top:10px;color: #486F73;',
                                       h2(strong("Nonprofit Environment Explorer"))
                                )
                        )),
                
                
                absolutePanel(
                        id = "layercontrols",
                        #class = "panel panel-default",
                        fixed = TRUE,
                        draggable = FALSE,
                        top = "0%",
                        left = "87.5%",
                        right = "2.5%",
                        bottom = "85%",
                        
                        
                            fluidRow(
                                    column(1, style='padding-top:15px;padding-right:0px;width: 100%',
  
                                           actionButton("help", label = "Tutorial  ", width = '100%',
                                                        icon = icon("question-circle")),
                                           br(), br(),
                                           actionButton("def", label = "Definitions", width = '100%',
                                                        icon = icon("book-open")
                                    )
                            ))
                        
                ),
                div(id = "filters",
                fluidRow(
                        column(1, style='width:0%;height:2%;padding-left:15px;padding-right:4px;'),
                        column(1,style='padding-left:1px;padding-right:0px;padding-bottom:1px;width:13.8%;',
                               div(id = "partner_help",
                                   pickerInput("partner", label= "Pionero Affiliation",inline=FALSE,multiple = TRUE, width= '100%',
                                               options = list(
                                                       `actions-box` = TRUE, 
                                                       size = 10,
                                                       `selected-text-format` = "count > 1",
                                                       `none-selected-text` = "None Selected"
                                               ),
                                               c("Partnered","Eligible","Not Eligible", "Discontinued Partnership", "No Information"),
                                               selected=c("Partnered")))),
                        
                        column(1,style='padding:0px;padding-left:2px;padding-right:0px;padding-bottom:1px;width:13.8%;height:2%;',
                               div(id = "category_help",
                                   pickerInput("category", label= "Theme Areas",inline=FALSE,multiple = TRUE,  width= '100%',
                                               options = list(
                                                       `actions-box` = TRUE, 
                                                       size = 10,
                                                       `selected-text-format` = "count > 3",
                                                       `count-selected-text`= "{0} Selected",
                                                       `none-selected-text` = "None Selected"
                                               ),
                                               c("Health","Education", "Community Development","Youth & Children", "Women & Girls",
                                                 "Human Rights" ,"Environment & Conservation","Animal Welfare","Security","Uncategorized"),
                                               selected=c("Health",
                                                          "Education",
                                                          "Community Development",
                                                          "Youth & Children",
                                                          "Women & Girls",
                                                          "Human Rights" ,
                                                          "Environment & Conservation",
                                                          "Animal Welfare",
                                                          "Security",
                                                          "Uncategorized")))),
                        
                        column(1,style='padding-left:2px;padding-right:0px;padding-bottom:1px;width:13.8%;height:2%;',
                               div(id = "size_help",
                                   pickerInput("sizevar",label = "Nonprofit Size", width= '100%',
                                               inline=FALSE,multiple = FALSE,
                                               options = list(
                                                   `actions-box` = TRUE, 
                                                   size = 10),
                                               choices = c(
                                                   "Annual Budget" = "budget_adj",
                                                   "Years Active" = "npo_age",
                                                   "None Selected" = "constant_size"),
                                               selected = "constant_size"))),
                        
                        column(1,style='padding-left:2px;padding-right:0px;padding-bottom:1px;width:13.8%;height:2%;',
                               div(id = "color_help",
                                   pickerInput("colorvar",label = "Nonprofit Color", width= '100%',
                                               inline=FALSE,multiple = FALSE,
                                               options = list(
                                                   `actions-box` = TRUE, 
                                                   size = 10),
                                               choices = c(
                                                   "Nonprofit Size" = "size",
                                                   "Tax Registration" = "Tax_Registration",
                                                   "Religious Affiliation" = "religious_aff",
                                                   "Guatemala Government Funded" = "guate_govt_funding",
                                                   "None Selected" ="constant_color"),
                                               selected = "constant_color"))),
                        
                        column(1,style='padding-left:2px;padding-right:0px;padding-bottom:1px;width:13.8%;height:2%;', 
                               div(id = "search_help",
                                   
                                   pickerInput("search",label = "Nonprofit Search", width= '100%',
                                               inline=FALSE,multiple = TRUE,
                                               options = list(
                                                   `actions-box` = TRUE,
                                                   `live-search` = TRUE,
                                                   `deselect-all-text` = "Disable Search",
                                                   size = 10
                                               ), 
                                               choices = names,
                                               choicesOpt = list(
                                                   content = stringr::str_trunc(names, width = 25)
                                               ),
                                               selected = "None Selected"))),
                        
                        
                        column(1,style='padding-left:2px;padding-right:0px;padding-bottom:1px;width:13.8%;height:2%;',
                               div(id = "demographic_help",
                                   pickerInput("demographics",label = "Demography", width= '100%',
                                               inline=FALSE,multiple = FALSE,
                                               options = list(
                                                   `actions-box` = TRUE,
                                                   `live-search` = TRUE,
                                                   size = 10),
                                               
                                               choices = list("Population" =list(
                                                   "Population"                               
                                                   , "Population Under 18"                      
                                                   , "Population 65 "                           
                                                   , "Indigenous Population"                    
                                                   , "Population in Urban Areas"                
                                                   , "Population with Personal ID"              
                                                   , "Population of Internal Migrants"          
                                                   , "Female lead Single Parent Households"     
                                                   , "People in Household" 
                                               ),
                                               Health = list(
                                                   "Gross Birth Rate"                         
                                                   , "Infant Mortality Rate"                    
                                                   , "Low Birth Weight Babies"                  
                                                   , "Access to Prenatal Care"                  
                                                   , "Access to Prenatal Vitamins"              
                                                   , "Medically Attended Births"                
                                                   , "Births Attended in Public Centers"        
                                                   , "Births Attended at Home"                  
                                                   , "Cesarian Section"                         
                                                   , "Gross Death Rate"                         
                                                   , "Death by External Causes"                 
                                                   , "Death by Diabetes"                        
                                                   , "Death by Diarrhea"                        
                                                   , "Death by Circulatory System Diseases"     
                                                   , "Death by Respiratory System Diseases"     
                                                   , "Death by Tuberculosis"                    
                                                   , "Death by HIV AIDS"        
                                               ),
                                               Education = list(
                                                   "Total Literacy Rate"                      
                                                   , "Male Literacy Rate"                       
                                                   , "Female Literacy Rate"                     
                                                   , "Youth Literacy Rate"                      
                                                   , "Number of Libraries"                      
                                                   , "Total Years of Schooling"                 
                                                   , "Female Years of Schooling"                
                                                   , "Youth Years of Schooling"                 
                                                   , "Time to Primary School"                   
                                                   , "Time to Secondary School"                 
                                                   , "Free Primary School Books"                
                                                   , "Borrowed or Gifted Secondary School Books"
                                                   , "Preprimary Net Enrollment Rate"           
                                                   , "Primary School Net Enrollment Rate"       
                                                   , "Middle School Net Enrollment Rate"        
                                                   , "High School Net Enrollment Rate"          
                                                   , "Enrollment in Higher Education"    
                                               ),
                                               Sanitation = list(
                                                   "Potable Water Access"                     
                                                   , "Improved Sanitation Access"
                                                   , "Home Water Access"                        
                                                   , "Homes Without Santitation Systems"
                                               ),
                                               Economy = list(
                                                   "Social Assistance Program Benefit"        
                                                   , "Poverty Rate"                             
                                                   , "Extreme Poverty Rate"
                                                   , "Female Paid Employees"                    
                                                   , "Total Employment Rate"      
                                               ),
                                               Security = list(
                                                   "Intrafamily Violence Rate"                
                                                   , "Homicide Rate"                            
                                                   , "Delinquent Injury Rate"                   
                                                   , "Robbery Rate"                             
                                                   , "Rape Rate"                                
                                                   , "Crimes Against Freedom Rate"              
                                                   , "Judicial Offense Rate"       
                                               ),
                                               Miscellaneous = list(
                                                   "Forest Fires"
                                                   ,"None Selected" = "Nothing Selected"
                                               )
                                               
                                               
                                               ),
                                               selected = "Nothing Selected"))),
                        
                        column(1,style='padding-left:2px;padding-right:0px;padding-bottom:1px;width:13.8%;height:2%;',
                               div(id = "department_help",
                                   pickerInput("depart_filters", label= "Departments",inline=FALSE, width= '100%',
                                               multiple = TRUE, 
                                               options = list(
                                                   `actions-box` = TRUE, 
                                                   size = 10,
                                                   `selected-text-format` = "count > 3",
                                                   `count-selected-text`= "{0} Selected",
                                                   `none-selected-text` = "None Selected"
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
                                                          "Santa Rosa","Zacapa", "Jalapa","Chiquimula","San Marcos"))))
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
        
        
        #######################################Tutorial 
        observeEvent(input$help,
                     introjs(session, options = list(
                             steps = data.frame(element = c("#filters  ",
                                                            "#demographic_help ", 
                                                            "#search_help ",
                                                            "#nonprofits "
                                                            
                                                            
                             ),
                             intro = c( includeMarkdown("tooltips/filters.md"),
                                        includeMarkdown("tooltips/demographic.md"),
                                        includeMarkdown("tooltips/search.md"),
                                        includeMarkdown("tooltips/nonprofits.md")
                                       
                             ),
                             position = c("auto",
                                          "auto",
                                          "auto",
                                          "auto"))
                             ),
                             
                     )
        )
        
        #######################################Definitions 
        observeEvent(
                        input$def, {
                                    showModal(modalDialog(
                                        title = "Definitions",
                                        includeHTML(knitr::knit2html("tooltips/definitions.md", fragment.only = TRUE)), #I had to knit it here because reading a full html file resets the styling
                                        easyClose = TRUE,
                                        size = "l",
                                        fade = TRUE
                    
                ))
            }
                     
        )
        
        #######################################Data for Circle markers 
        data <- reactive({
                
                if (is.null(input$search)) {
                        plot %>% 
                                filter(category %in% input$category) %>%
                                filter(partner_status %in% input$partner) %>%
                                filter(department%in% input$depart_filters)
                        
                }
                else if (input$search == "None Selected") {
                    plot %>% 
                        filter(category %in% input$category) %>%
                        filter(partner_status %in% input$partner) %>%
                        filter(department%in% input$depart_filters)
                    
                }
                else {
                        filter(plot, npo %in% input$search)
                }
        })
        
        #######################################Data for Departments 
        
        demographic <- reactive({
                demographic_map %>% dplyr::filter(measure == input$demographics)     
        })        
        
 
        
        pal2 <- reactive({ 
            
            if (input$demographics %in% "Nothing Selected"){ #Separate colorpalette for "Nothing Selected"
                
                colorNumeric(palette = colorRampPalette(c("#193A45"))(length(demographic()$value)),
                             domain = 1,
                             na.color = "#193A45")
            }
            
            else {
                    colorNumeric(
                        palette = colorRampPalette(c("#F2F2F2","#FFC000", "#193A45"))(length(demographic()$value)),
                        domain = demographic()$value,
                        na.color = "#808080") # Pionero color: #486F73 #193A45 #FFC000 #F2F2F2 #BFBFBF 
            }
        }) 
        
        
        output$map <- renderLeaflet({
                leaflet(data = demographic(), 
                        options = leafletOptions(
                                attributionControl=FALSE)) %>%
                        addTiles(
                                urlTemplate = "https://tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=aae485d383324e008257aab3f9467916",
                                attribution = 'Imagery from <a href="http://giscience.uni-hd.de/">GIScience Research Group @ University of Heidelberg</a> 
                                | Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors', 
                                options = tileOptions(minZoom = 0, maxZoom = 18)
                        ) %>% setView(-90.352651, 15.8, zoom = 8)
                
                
        })
        
        
        
        
        #######################################Observer Function for Circle Markers & Polygons
        observe({if (nrow(data()) != 0) {
                
                
                colorBy <- input$colorvar
                sizeBy <- input$sizevar
                colorData <- data()[[colorBy]]
                x <-data()[[sizeBy]] 
                
                #I had to create a separate color palette for basically every color variable 
                # "#F2F2F2", "#BFBFBF","#FFC000", "#486F73","#193A45"
                
                if (input$colorvar %in% "constant_color"){
                    
                 pal <-   colorFactor(palette = colorRampPalette(c("#F2F2F2"),space = "Lab")(length(colorData)),
                                domain = colorData,
                                reverse = TRUE,
                                na.color = "#F2F2F2")
                    
                } else if (input$colorvar %in% "size" | input$colorvar %in% "guate_govt_funding" ) {
                pal <- colorFactor(palette = colorRampPalette(c("#193A45","#486F73","#BFBFBF","#FFC000","#F2F2F2"),space = "Lab")(length(colorData)),
                                   domain = colorData,
                                   reverse = TRUE,
                                   na.color = "#BFBFBF")
                
                }else if (input$colorvar %in% "Tax_Registration") {
                    pal <- colorFactor(palette = colorRampPalette(c("#F2F2F2", "#BFBFBF","#FFC000", "#486F73","#193A45"),space = "Lab")(length(colorData)),
                                       domain = colorData,
                                       reverse = TRUE,
                                       na.color = "#BFBFBF")
                    
                }else if (input$colorvar %in% "religious_aff" ) {
                    pal <- colorFactor(palette = colorRampPalette(c("#F2F2F2", "#BFBFBF","#FFC000", "#486F73","#193A45"),space = "Lab")(length(colorData)),
                                       domain = colorData,
                                       reverse = TRUE,
                                       na.color = "#BFBFBF")
                }
                
                
                varname <- switch(
                        input$colorvar,
                        "constant" = "All Nonprofits",
                        "size" = "Nonprofit Size",
                        "faith_based"= "Faith Based")
                
                
                
                
                #setting a different size when partners are the only ones selected
                if (unique(data()$partner_status) %in% "Partnered"){
                    
                    size <-sqrt(x / mean(x) * 100)    
                    
                } else {
                    size <-sqrt(x / mean(x) * 50) 
                }
                
                
                leafletProxy("map") %>% 
                        clearShapes() %>%
                        clearControls() %>%
                        addPolygons(data = Guatemala,
                                    stroke = TRUE,
                                    smoothFactor = 1,
                                    weight = 3, 
                                    color = "Black",
                                    fillOpacity = 0) %>% 
                        addPolygons(    data = demographic(),
                                        stroke = TRUE,
                                        smoothFactor = 1,
                                        fillColor = ~ pal2()(demographic()$value),
                                        fillOpacity = .5,
                                        weight = 2.5, 
                                        color = "black",
                                        highlight = highlightOptions(
                                                weight = 5,
                                                fillOpacity = 0.7,
                                                bringToFront = FALSE),
                                        label = ~ paste0("Department: ", Department),
                                        popup =  ~ paste0(
                                                "<h4/><b>",Department,"</b><h5/>",
                                                "<h5/><b>",input$demographics,sep = " ",demographic()$formatted,sep = " ",demographic()$units,
                                                "<h5/>","Population: ",sep = " ", demographic()$Population,
                                                "<h5/>","Poverty Rate: ",sep = " ", demographic()$Poverty.Rate, "%",
                                                "<h5/>","Literacy Rate: ",sep = " ", demographic()$Total.Literacy.Rate, "%",
                                                "<h5/>","Homicide Rate: ",sep = " ", demographic()$Homicide.Rate, "%",
                                                "<h5/>","Employment Rate: ",sep = " ", demographic()$Total.Employment.Rate, "%",
                                                "<h5/>","Improved Sanitation Access: ",sep = " ", demographic()$Improved.Sanitation.Access, "%",
                                                "<h5/>","Gross Birth Rate: ",sep = " ", demographic()$Gross.Birth.Rate, "%"

                                                ),
                                        group = "Demographic Data") %>%
                        clearMarkers() %>% #you have to clear previously drawn markers
                        addCircleMarkers(data = data(), lng =  ~ longitude,lat =  ~ latitude,
                                stroke = TRUE,
                                weight = 2,
                                color = "black",
                                popup =  ~ paste0(
                                            "<h4/><b>",website,"</b><h5/>",
                                            "<h5/>","Partner Status: ",sep = " ",partner_status,
                                            "<h5/>","Eligibility Restrictions: ",sep = " ", ne_dp_reason,
                                            "<h5/>","Nonprofit Size: ",sep = " ",size,
                                            "<h5/>","Year Founded: ",sep = " ",year_founded,
                                            "<h5/>","Annual Budget: $",format(budget, big.mark=","),
                                            "<h5/>","Theme Areas: ",sep = " ",list_categories,
                                            "<h5/>","Religious Affiliation: ",sep = " ",religious_aff,
                                            "<h5/>", "Tax Registration: ", sep = " ", Tax_Registration,
                                            "<h5/>", "Receives Guatemalan Government Funds: ", sep = " ",guate_govt_funding),
                                label = ~ paste0("Nonprofit: ", sep = " ", npo),
                                radius = size,
                                fillOpacity = 0.6,
                                fillColor =  ~ pal(colorData),
                                group = "Nonprofit Data"
                        )
                
                #######################################Legends based on which layer checkboxes are ticked
                if (("Nothing Selected" %in% input$demographics) & ("constant_color" %in% input$colorvar)) {
                    leafletProxy("map")
                }
                
                else if (("Nothing Selected" %in% input$demographics)) {
                        leafletProxy("map") %>% 
                                addLegend(data = data(), "bottomright",pal = pal, values = colorData, title = varname,
                                          group = "Nonprofit Data", layerId = "nonleg")
                }
                else if (!("Nothing Selected" %in% input$demographics)) {
                        leafletProxy("map") %>% 
                            addLegend(data = demographic(), "bottomright", pal = pal2(), values = ~demographic()$value,
                                      opacity = 0.7, title = ~ paste0(unique(measure)," ",unique(units)), group = "Demographic Data", layerId = "demleg") %>% 
                            addLegend(data = data(), "bottomright",pal = pal, values = colorData, title = varname,
                                      group = "Nonprofit Data", layerId = "nonleg")
                }
                
                
        
                else{leafletProxy("map") %>% clearMarkers()} #clear the map if the data() is empty
                
                
        }    
                
                
                
        })
        
        output$num_matching <-renderText({format(nrow(unique(dplyr::select(data(), npo))), big.mark = ",")})
        
        
})
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

shinyApp(ui, server)