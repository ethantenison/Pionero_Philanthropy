# After running the app once, to deploy follow these steps: 
# 1. rsconnect::setAccountInfo(name='pionero-philanthropy', token='8D1104F33B82061B83FFF9C8E64AC9FE', secret='n4ffBkrtcoJSEv2FhCUhfpR9d968t7kcMTAnmCJi')
# 2. rsconnect::deployApp(appName = "Nonprofits_Explorer")

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
           "ggplot2",
           "dplyr",
           "readr",
           "rsconnect"
           
        )

new.packages <-list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]


library(shiny)
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
library(rsconnect)



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
options(scipen = 999)


plot <- readRDS("./data/npo_data.rds")

names <- dplyr::select(plot, npo)
names[nrow(names) + 1,] = c("None Selected")
names <- unique(names$npo)


guatemala.shape_orig <- st_read("data/GTM_adm0.shp", stringsAsFactors = FALSE)
Guatemala <-st_transform(guatemala.shape_orig,"+proj=longlat +ellps=WGS84 +datum=WGS84")
guatemala.shape_orig2 <- st_read("data/GTM_adm1.shp", stringsAsFactors = FALSE)
Guatemala_department <-st_transform(guatemala.shape_orig2,"+proj=longlat +ellps=WGS84 +datum=WGS84")

demographic_map <- readRDS("./data/demographic_map.rds")
demographic_map <- as.data.frame(demographic_map)
demographic_map <- st_as_sf(demographic_map)
demographic_map <- st_transform(demographic_map,"+proj=longlat +ellps=WGS84 +datum=WGS84")
demographic_map <- mutate(demographic_map, formatted = as.character(format(value,  big.mark=",", digits=1)))
demographic_map <- mutate(demographic_map, Population = as.character(format(Population,  big.mark=",", digits=0)))
demographic_map <- mutate(demographic_map, Total.Literacy.Rate = as.character(format(Total.Literacy.Rate,  big.mark=",", digits=1)))


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
                HTML('<meta name="viewport" content="width=1024">'),
                shinyjs::useShinyjs(),
                introjsUI(),
                
                #####Styling Tags
                tags$head(includeCSS("www/css/bootstrap.css"),
                          includeScript("www/js/google_analytics.js"),
                          tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css",
                                    integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous"),
                          tags$script(src = "www/js/wordwrap.js"),
                          tags$style(".checkbox-inline {margin: 0 !important;}"),
                          tags$style(".selectize-dropdown {top: -200px !important;}"),
        
                          tags$style(".selectize-input {background: #193A45 ; border-color: #193A45 ; color: #F2F2F2;}"),
                          tags$style(".bs-placeholder {color: #F2F2F2 !important;}"),
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
                
                tags$style(".pretty.p-default input:dropdown~.state label:after {background-color: #486F73 !important;}"), #change color checkbox widgets
                
                ######Shiny App UI Elements 
                leafletOutput("map", width = "100%", height = "91.5%"),
                
                absolutePanel(
                        id = "topbar",
                        fixed = TRUE,
                        draggable = FALSE,
                        top = "0%",
                        left = "2.5%",
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
                        fixed = TRUE,
                        draggable = FALSE,
                        top = "0%",
                        left = "87.5%",
                        right = "2.5%",
                        bottom = "85%",
                        
                        
                            fluidRow(
                                    column(1, style='padding-top:20px;padding-right:0px;width: 100%',
                                           
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
                                                       `count-selected-text`= "{0} Selected",
                                                       `none-selected-text` = "None Selected"
                                               ),
                                               c("Partner" = "Partnered","Eligible","Not Eligible", "Discontinued Partnership", "No Information"),
                                               selected=c("Partnered","Eligible","Not Eligible", "Discontinued Partnership", "No Information")))),
                        
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
                                                 "Human Rights" ,"Environment & Conservation","Animal Welfare","Security","Theme not available" = "Uncategorized"),
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
                                                   "Years Active" = "npo_age_adj",
                                                   "Total Evaluation Score" = "total_eval_score_adj",
                                                   "Efficiency" = "efficiency_adj",
                                                   "Transparency" = "transparency_adj",
                                                   "Relevance" = "relevance_adj",
                                                   "Impact" = "impact_adj",
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
                                                   "Guatemalan Government Funded" = "guate_govt_funding",
                                                   "Seal of Excellence" = "seal_excellence",
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
                                                   `none-selected-text` = "None Selected", 
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
                                               
                                               choices = 
                                                 list(
                                                   Education = list(
                                                     
                                                       "Borrowed or Gifted Secondary School Books"                      
                                                     , "Enrollment in Higher Education"                       
                                                     , "Female Literacy Rate"
                                                     , "Female Years of Schooling"
                                                     , "Free Primary School Books"
                                                     , "High School Net Enrollment Rate"
                                                     , "Male Literacy Rate"
                                                     , "Middle School Net Enrollment Rate"
                                                     , "Number of Libraries"  
                                                     , "Preprimary Net Enrollment Rate"           
                                                     , "Primary School Net Enrollment Rate"       
                                                     , "Time to Primary School"                   
                                                     , "Time to Secondary School"
                                                     ,"Total Literacy Rate"
                                                     ,"Total Years of Schooling"
                                                     ,"Youth Literacy Rate"
                                                     , "Youth Years of Schooling"
                                                   ),
                                                   
                                                   Economy = list(
                                                       "Extreme Poverty Rate"
                                                     , "Female Paid Employees"
                                                     , "Social Assistance Program Benefit"        
                                                     , "Poverty Rate"                             
                                                     , "Total Employment Rate"      
                                                   ),
                                                   
                                                   Health = list(
                                                       "Access to Prenatal Care"                  
                                                     , "Access to Prenatal Vitamins"
                                                     , "Births Attended in Public Centers"        
                                                     , "Births Attended at Home"                  
                                                     , "Cesarian Section"
                                                     , "Death by External Causes"                 
                                                     , "Death by Diabetes"                        
                                                     , "Death by Diarrhea"                        
                                                     , "Death by Circulatory System Diseases"     
                                                     , "Death by Respiratory System Diseases"     
                                                     , "Death by Tuberculosis"                    
                                                     , "Death by HIV AIDS"
                                                     , "Gross Birth Rate"
                                                     , "Gross Death Rate"
                                                     , "Infant Mortality Rate"                    
                                                     , "Low Birth Weight Babies"                  
                                                     , "Medically Attended Births"                
         
                                                   ),
                                                   
                                                   "Population" =list(
                                                     "Female-led Single Parent Households"
                                                   , "Indigenous Population"
                                                   , "People in Household"
                                                   , "Population" 
                                                   , "Population in Urban Areas"
                                                   , "Population of Internal Migrants"
                                                   , "Population Under 18"
                                                   , "Population with Personal ID"
                                                   , "Population 65+"                           
      
                                               ),
                                               
                                               
                                               Sanitation = list(

                                                     "Home Water Access"
                                                   , "Homes without Santitation System"
                                                   , "Improved Sanitation Access"
                                                   , "Potable Water Access"
                                               ),
                                               
                                               Security = list(
                                                     "Crimes Against Freedom Rate"
                                                   , "Delinquent Injury Rate"
                                                   , "Homicide Rate"
                                                   , "Intrafamily Violence Rate"                
                                                   , "Robbery Rate"                             
                                                   , "Rape Rate"                                
                                                   , "Judicial Offense Rate"       
                                               ),
                                               Miscellaneous = list(
                                                   "Forest Fires"
                                                   ,"None Selected" = "None Selected"
                                               )
                                               
                                               
                                               ),
                                               selected = "None Selected"))),
                        
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
                                                   c("Alta Verapaz","Baja Verapaz","Chimaltenango","Chiquimula","El Progreso","Escuintla","Guatemala",
                                                     "Huehuetenango","Izabal","Jalapa","Jutiapa","Petén", "Quetzaltenango", "Quiché","Retalhuleu",    
                                                     "Sacatepéquez","San Marcos", "Santa Rosa", "Sololá","Suchitepéquez", "Totonicapán","Zacapa"),
                                               
                                               
                                               selected=c("Alta Verapaz","Baja Verapaz","Chimaltenango","Chiquimula","El Progreso","Escuintla","Guatemala",
                                                          "Huehuetenango","Izabal","Jalapa","Jutiapa","Petén", "Quetzaltenango", "Quiché","Retalhuleu",    
                                                          "Sacatepéquez","San Marcos", "Santa Rosa", "Sololá","Suchitepéquez", "Totonicapán","Zacapa"))))
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



server <- shinyServer(function(input, output, session) {
        
        #Tutorial and Definitions markdown files are located in the tooltips folder
        #In order to make addition tutorial steps you must create a markdown file and reference the element ID in the UI
        #######################################Tutorial 
        observeEvent(input$help,
                     introjs(session, options = list( #Include ids of elements to be selected here
                             steps = data.frame(element = c("#filters  ",
                                                            "#search_help ",
                                                            "#demographic_help ",
                                                            "#nonprofits "
                                                            
                                                            
                             ),
                             intro = c( includeMarkdown("tooltips/filters.md"),
                                        includeMarkdown("tooltips/search.md"),
                                        includeMarkdown("tooltips/demographic.md"),
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
                                        includeHTML(knitr::knit2html("tooltips/definitions.md", fragment.only = TRUE)), #must knit
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
            
            if (input$demographics %in% "None Selected"){ #Separate colorpalette for "None Selected"
                
                colorNumeric(palette = colorRampPalette(c("#193A45"))(length(demographic()$value)),
                             domain = 1,
                             na.color = "#193A45")
            }
            
            else {
                    colorNumeric(
                        palette = colorRampPalette(c("#F2F2F2","#FFC000", "#193A45"))(length(demographic()$value)),
                        domain = demographic()$value,
                        na.color = "#808080") 
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
                
                
                ######Nonprofit Size, size baseline is changed based on if only partners are selected or not
                sizeBy <- input$sizevar
                x <-data()[[sizeBy]] 

                if (unique(data()$partner_status) %in% "Partnered"){
                    
                    size <-sqrt(x / mean(x) * 100)    
                    
                } else {
                    size <-sqrt(x / mean(x) * 50) 
                }
                
                #####Nonprofit Color, palettes change depending on what color variable is selected
                colorBy <- input$colorvar
                colorData <- data()[[colorBy]]
                
                if (input$colorvar %in% "constant_color"){
                    
                 pal <-   colorFactor(palette = colorRampPalette(c("#F2F2F2"),space = "Lab")(length(colorData)),
                                domain = colorData,
                                reverse = TRUE,
                                na.color = "#F2F2F2")
                    
                } else if (input$colorvar %in% "size" | input$colorvar %in% "guate_govt_funding" ) {
                pal <- colorFactor(palette = colorRampPalette(c("#193A45","#486F73","#BFBFBF","#F2F2F2", "#FFC000"),space = "Lab")(length(colorData)),
                                   domain = colorData,
                                   reverse = TRUE,
                                   na.color = "#BFBFBF")
                
                }else if (input$colorvar %in% "Tax_Registration" & unique(data()$Tax_Registration) == c("Guatemala only","US & Guatemala","US only" )) { 
                  pal <- colorFactor(palette = colorRampPalette(c("#F2F2F2","#FFC000","#193A45"),space = "Lab")(length(colorData)),
                                     domain = colorData,
                                     reverse = TRUE,
                                     na.color = "#BFBFBF")
                  
                }else if (input$colorvar %in% "Tax_Registration") {
                    pal <- colorFactor(palette = colorRampPalette(c("#F2F2F2","#FFC000", "#486F73","#193A45"),space = "Lab")(length(colorData)),
                                       domain = colorData,
                                       reverse = TRUE,
                                       na.color = "#BFBFBF")
                    
                }else if (input$colorvar %in% "religious_aff" ) {
                    pal <- colorFactor(palette = colorRampPalette(c("#F2F2F2", "#BFBFBF","#FFC000", "#486F73","#193A45"),space = "Lab")(length(colorData)),
                                       domain = colorData,
                                       reverse = TRUE,
                                       na.color = "#BFBFBF")
                }
                else if (input$colorvar %in% "seal_excellence" ) {
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
                
                
                #####Joining map elements together 
                
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
                                                "<h5/><b>",input$demographics,sep = ": ",demographic()$formatted,sep = "",demographic()$units,
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
                                            "<h5/>","Pionero Affiliation: ",sep = " ",partner_status_adj,
                                            "<h5/>","Restrictions: ",sep = " ", ne_dp_reason,
                                            "<h5/>","Seal of Excellence: ",sep = " ",seal_excellence,
                                            "<h5/>","Total Evaluation Score: ",sep = " ",total_eval_score,
                                            "<h5/>","Nonprofit Size: ",sep = " ",size_adj,
                                            "<h5/>","Year Founded: ",sep = " ",year_founded,
                                            "<h5/>","Annual Budget: $",format(budget, big.mark=","),
                                            "<h5/>","Theme Areas: ",sep = " ",list_categories,
                                            "<h5/>","Religious Affiliation: ",sep = " ",religious_aff_adj,
                                            "<h5/>", "Tax Registration: ", sep = " ", Tax_Registration,
                                            "<h5/>", "Tax Details: ", sep = " ", tax_details,
                                            "<h5/>", "Receives Guatemalan Government Funds: ", sep = " ",guate_govt_funding),
                                label = ~ paste0("Nonprofit: ", sep = " ", npo),
                                radius = size,
                                fillOpacity = 0.6,
                                fillColor =  ~ pal(colorData),
                                group = "Nonprofit Data"
                        )
                
                #######################################Legends based on which color variables are shown
                if (("None Selected" %in% input$demographics) & ("constant_color" %in% input$colorvar)) {
                    leafletProxy("map")
                }
                
                else if (("None Selected" %in% input$demographics)) {
                        leafletProxy("map") %>% 
                                addLegend(data = data(), "bottomright",pal = pal, values = colorData, title = varname,
                                          group = "Nonprofit Data", layerId = "nonleg")
                }
                else if (!("None Selected" %in% input$demographics)) {
                        leafletProxy("map") %>% 
                            addLegend(data = demographic(), "bottomright", pal = pal2(), values = ~demographic()$value,
                                      opacity = 0.7, title = ~ paste0(unique(measure)," ",unique(units)), group = "Demographic Data", layerId = "demleg") %>% 
                            addLegend(data = data(), "bottomright",pal = pal, values = colorData, title = varname,
                                      group = "Nonprofit Data", layerId = "nonleg")
                }
                
                
        
                else{leafletProxy("map") %>% clearMarkers()} #clear the map if the data() is empty
                
                
        }    
                
                
                
        })
        
        #####Outputs the number of nonprofits Selected
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