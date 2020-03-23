



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

demographic_map <- mutate(demographic_map, units = "")

for (i in 1:length(demographic_map$units)){
        if(demographic_map$measure[i] == "population"){        
                demographic_map$units[i] <- "Inhabitants"               
        }
        else if(demographic_map$measure[i] == "poverty"){        
                demographic_map$units[i] <- "% Below Poverty Line"              
        }
        else if(demographic_map$measure[i] == "extreme_poverty"){        
                demographic_map$units[i] <- "% Below Extreme Poverty Line"              
        }
        else if(demographic_map$measure[i] == "gross_birth_rate"){        
                demographic_map$units[i] <- "Births per 1,000 inhabitants"              
        }
        else if(demographic_map$measure[i] == "infant_mortality_rate"){        
                demographic_map$units[i] <- "Deaths per 1,000 births"  
        }
        else if(demographic_map$measure[i] == "low_birth_weight"){        
                demographic_map$units[i] <- "% births less than 2.5 kg"              
        }
        else if(demographic_map$measure[i] == "medically_attended_births"){        
                demographic_map$units[i] <- "% births attended by medics"              
        }
        else if(demographic_map$measure[i] == "public_center_births"){        
                demographic_map$units[i] <- "% births in medical centers"              
        }
        else if(demographic_map$measure[i] == "gross_death_rate"){        
                demographic_map$units[i] <- "Deaths per 1,000 inhabitants"              
        }
        else if(demographic_map$measure[i] == "death_external"){        
                demographic_map$units[i] <- "unknown units"              
        }
        else if(demographic_map$measure[i] == "death_diabetes"){        
                demographic_map$units[i] <- "Deaths from diabetes per 1,000 deaths"              
        }
        else if(demographic_map$measure[i] == "death_diarrhea"){        
                demographic_map$units[i] <- "# of deaths from diarrhea per 1,000 deaths"              
        }
        else if(demographic_map$measure[i] == "death_circulatory"){        
                demographic_map$units[i] <- "Deaths from ciruculatory illnesses per 1,000 deaths"              
        }
        else if(demographic_map$measure[i] == "death_respiratory"){        
                demographic_map$units[i] <- "Deaths from respiratory illnesses per 1,000 deaths"              
        }
        else if(demographic_map$measure[i] == "death_tuber"){        
                demographic_map$units[i] <- "Tuberculosis deaths per 1,000 deaths"              
        }
        else if(demographic_map$measure[i] == "death_hivaids"){        
                demographic_map$units[i] <- "HIV/AIDS deaths per 1,000 deaths"              
        }
        else if(demographic_map$measure[i] == "intrafamily_violence_rate" ){        
                demographic_map$units[i] <- "% who have experienced domestic violence"              
        }
        else if(demographic_map$measure[i] == "homicide_rate"){        
                demographic_map$units[i] <- "Homicides per 100,000 inhabitants"              
        }
        else if(demographic_map$measure[i] == "delinquent_injury_rate"){        
                demographic_map$units[i] <- "Delinquent Injuries per 100,000 inhabitants"              
        }
        else if(demographic_map$measure[i] == "robbery_rate"){        
                demographic_map$units[i] <- "Robberies per 100,000 inhabitants"              
        }
        else if(demographic_map$measure[i] == "rape_rate"){        
                demographic_map$units[i] <- "Rapes per 100,000 inhabitants"              
        }
        else if(demographic_map$measure[i] == "crimes_against_freedom_rate"){        
                demographic_map$units[i] <- "Unknown per 100,000 inhabitants"              
        }
        else if(demographic_map$measure[i] == "judicial_offense_rate"){        
                demographic_map$units[i] <- "Judicial Offenses per 100,000 inhabitants"              
        }
        else if(demographic_map$measure[i] == "literacy_rate15_24"){        
                demographic_map$units[i] <- "% Literate ages 15-24"              
        }
        else if(demographic_map$measure[i] == "female_paid_employees"){        
                demographic_map$units[i] <- "% Females paid employees"              
        }
        else if(demographic_map$measure[i] == "employment_rate"){        
                demographic_map$units[i] <- "% Employed"              
        }
        else if(demographic_map$measure[i] == "potable_water_access"){        
                demographic_map$units[i] <- "% access to portable water"              
        }
        else if(demographic_map$measure[i] == "improved_sanitation_access"){        
                demographic_map$units[i] <- "% access to improved sanitation"              
        }
        else if(demographic_map$measure[i] == "forest_fires"){        
                demographic_map$units[i] <- "# of Forest Fires"              
        }
        else if(demographic_map$measure[i] == "home_water_access"){        
                demographic_map$units[i] <- "% with water access at home"              
        }
        else if(demographic_map$measure[i] == "homes_no_santitation"){        
                demographic_map$units[i] <- "% with no sanitation at home"              
        }
        
}

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
                shinyjs::useShinyjs(),
                introjsUI(),
                tags$head(includeCSS("www/css/bootstrap.css")),
                tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                tags$head(includeScript("www/js/google_analytics.js")),
                tags$head(tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css",
                                    integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")),
                tags$head(tags$script(src = "www/js/wordwrap.js")),
                tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
                tags$head(tags$style(HTML('.form-group, .selectize-control {margin-bottom: 0px;}.box-body {padding-bottom: 0px;}'))), #changes marginscheckboxes
                tags$style(".pretty.p-default input:checked~.state label:after {background-color: #A7E0AC !important;}"), #change color widgets 
                
                
                leafletOutput("map", width = "100%", height = "100%"),
                
                # fluidRow(
                #         column(3,
                #         ),
                #         
                #         column(4,
                #         ),
                #         
                #         column(3,
                # )
        
                
                absolutePanel(
                        id = "controls",
                        class = "panel panel-default",
                        fixed = TRUE,
                        draggable = FALSE,
                        top = "2.5%",
                        left = "auto",
                        right = "2.5%",
                        bottom = "20%",
                        
                        width = 275,
                        height = "auto",
                        
                        
                        ######################################Sidebar header and nummatching
                        fluidRow(column(6, offset = 1, style='padding:0px;padding-bottom:0px;',
                                        h3("Nonprofit Explorer v1.4"))),
                        
                        fluidRow(column( 8, offset = 1, style='padding:0px; top:0px;margin-top:-1.5em',h3("(",textOutput("num_matching", inline = TRUE),"selected)")
                        )),
                        
                        
                        ######################################Tutorial Button 
                        fluidRow(column(8, offset =1 , style= 'padding:2px;',actionButton(
                                "help", "Tutorial", icon = icon("book-open", class = "fa-pull-left"),
                                style="color: #555555;border-color: #bcbcbc; background: #fff",
                                width = "100%"))),
                        
                        
                        
                        ##################check boxes for nonprofit categories
                                            fluidRow(
                                                    column(6,offset = 1, style='padding:2px;color: #555555;',
                                                           pickerInput("category", label= h4("Category Filters"),inline=FALSE,multiple = TRUE, 
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
                                                                                          "Crime")),
                                                           
                                                           pickerInput("depart_filters", label= h4("Department Filters"),inline=FALSE,
                                                                       ,multiple = TRUE, 
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
                                                                                  "Santa Rosa","Zacapa", "Jalapa","Chiquimula","San Marcos"))
                                                           
                                                           )
                                                    
                                            ),
                        
                        #######################################Other Options
                        h5("Other Filters", a(id = "toggleother", "show/hide")),
                        shinyjs::hidden(div(id = "filterother",
                                            fluidRow(column(6, offset = 1, style='padding:2px;', h4("Other Filters"))),
                                            
                                            fluidRow(
                                                    column(4, offset = 1, style='padding:2px; color: #555555;',#prettyCheckbox("na_select", "Include NAs", TRUE),
                                                           prettyCheckbox("faith", "Faith Based Only", FALSE), prettyCheckbox("parnters", "Partners Only", TRUE))
                                            ))),
                        
                        #######################################graph controls
                        tags$hr(),
                        fluidRow(column(5, offset = 1,style='padding:2px;',selectInput("sizevar","Size Variable:",
                                                                                       choices = c(
                                                                                               "Annual Budget" = "budget_adj",
                                                                                               "Same Size" = "constant",
                                                                                               "Years Active" = "npo_age"),
                                                                                       selected = "constant")),
                                 
                                 column(5, style='padding:2px;',selectInput("colorvar","Color Variable:",
                                                                            choices = c(
                                                                                    "Nonprofit Size" = "size",
                                                                                    "Partner Status" = "partner_status",
                                                                                    "Same Color" ="constant"),
                                                                            selected = "constant"))),
                        
                        #######################################Search bar
                        fluidRow(column( 10,offset = 1, style='padding:2px;',selectizeInput(
                                "search",
                                label = "Search Name: ",
                                choices = plot$npo,
                                selected = NULL,
                                multiple = FALSE,
                                options = list(
                                        placeholder = 'Select a nonprofit by name',
                                        onInitialize = I('function() { this.setValue(""); }'))))),
                        
                        #######################################Demographic filter
                        fluidRow(column( 10,offset = 1, style='padding:2px;',selectInput(
                                "demographics",
                                label = "Change Demography by Department",
                                choices = unique(demographic_map$measure),
                                selected = "poverty",
                                multiple = FALSE))
                        ),       
                        
                        ####################################### Histogram of Budget
                        fluidRow(column(10, offset = 1, style='padding:0px;',plotOutput("histBudget", height = 200))))))


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
        
        #######################################Data for Circle markers 
        data <- reactive({
                
                if (input$search == "") {
                        plot %>% 
                                filter(category %in% input$category) %>% filter(.,
                                                                                
                                                                                # if(input$na_select) { 
                                                                                #         is.na(budget) | budget
                                                                                # } else { 
                                                                                #          !is.na(budget)
                                                                                # },
                                                                                # 
                                                                                # if(input$na_select) { 
                                                                                #         is.na(year_founded) | year_founded
                                                                                # } else { 
                                                                                #         !is.na(year_founded)
                                                                                # },
                                                                                
                                                                                if(input$faith) {
                                                                                        faith_based == "Faith Based"
                                                                                } else { 
                                                                                        faith_based == "Faith Based" | is.na(faith_based) 
                                                                                },
                                                                                
                                                                                if(input$parnters) {
                                                                                        partner_status == "Partnered"
                                                                                } else { 
                                                                                        partner_status == "Partnered" | partner_status == "Eligible" | 
                                                                                                partner_status == "Discontinued Partnership" | partner_status == "Not Eligible" |
                                                                                                is.na(partner_status)
                                                                                }
                                ) %>% filter(department %in% input$depart_filters)
                        
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
                palette = "PuBu",
                domain = demographic()$value)
        }) 
        
        
        output$map <- renderLeaflet({
                leaflet(data = demographic()) %>%
                        addTiles() %>%
                        fitBounds( -90.74078, 13.52793, -88.0067, 17.78793) 
        })
        
        
        #######################################Histogram settings
        his <- plot[!is.na(plot$budget), ]
        BudgetBreaks <-hist(plot = FALSE, his$budget, breaks = 5)$breaks
        output$histBudget <- renderPlot({
                # If no zipcodes are in view, don't plot
                if (nrow(data()) == 0)
                        return(NULL)
                hist(
                        his$budget,
                        breaks = BudgetBreaks,
                        main = "Partner Budget Distribution",
                        xlab = "Annual Budget",
                        xlim = range(his$budget),
                        ylab = "Number of Nonprofits",
                        col = '#A7E0AC',
                        border = 'white'
                )
        })
        
        #######################################Observer Function for Circle Markers 
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
                size <-sqrt(x / quantile(x, 0.95, na.rm = TRUE) * 100)
                
                
                leafletProxy("map") %>% 
                        addPolygons(    data = demographic(),
                                        stroke = TRUE,
                                        smoothFactor = 1,
                                        fillColor = ~ pal2()(demographic()$value),
                                        fillOpacity = .4,
                                        weight = 3, 
                                        color = "black",
                                        highlight = highlightOptions(
                                                weight = 5,
                                                fillOpacity = 0.7,
                                                bringToFront = FALSE),
                                        popup =  ~ paste0(
                                                "<h4/><b>",department,"</b><h5/>","Measure: ",sep = " ",input$demographics,
                                                "<h5/>","Value: ",sep = " ",demographic()$value)) %>%
                        clearMarkers() %>% #you have to clear previously drawn markers
                        addCircleMarkers(data = data(), lng =  ~ longitude,lat =  ~ latitude,stroke = FALSE,popup =  ~ paste0(
                                "<h4/><b>",npo,"</b><h5/>","Parnter Status: ",sep = " ",partner_status,
                                "<h5/>","Nonprofit Size: ",sep = " ",size,
                                "<h5/>","Year Founded: ",sep = " ",year_founded,
                                "<h5/>","Annual Budget: $",sep = " ",budget,
                                "<h5/>","Website: ",sep = " ",website,
                                "<h5/>","All Categories: ",sep = " ",list_categories),
                                
                                label = ~ paste0("Nonprofit: ", sep = " ", npo),
                                radius = size,
                                fillOpacity = 0.5,
                                color = "black",
                                fillColor =  ~ pal(colorData)
                        ) %>%
                        clearControls() %>% 
                        addLegend(data = data(), "bottomleft",pal = pal, values = colorData, title = varname) %>%
                        addLegend(data = demographic(), "bottomleft", pal = pal2(), values = ~demographic()$value,
                                  opacity = 0.7, title = ~ paste0(unique(units)))
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