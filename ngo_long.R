#Map of Education Non-profits in Guatemala 


# load libraries
library(raster)
library(sp)
library(sf)
library(labelled)
library(rgdal)
library(rio)
library(haven)
library(tidyverse)
library(geosphere) # library with lots of shape files
library(mapview) # incredible interactive map visualization in R
library(openxlsx)
library(janitor)
library(stringr)
library(tidyr)
library(googlesheets)

# read the shape file using sf package
                guatemala.shape_orig <- st_read("Guatemala_shape_files/GTM_adm1.shp", stringsAsFactors=FALSE)

# transform it to WGS84 coordinate system
                Guatemala <- st_transform(guatemala.shape_orig, "+proj=longlat +ellps=WGS84 +datum=WGS84")


#calling in the excel file 
                db <- read.xlsx("ngo_database.xlsx", sheet = 3, startRow = 1, colNames =TRUE)
                db <- clean_names(db)
                db$name <- as.character(db$name)
                db$focus_area_s <- as.character(db$focus_area_s)
                db$focus_area_s <- paste("All Nonprofits, ", db$focus_area_s, sep =" " )
                Encoding(db$name) <- "UTF-8"
                Encoding(db$focus_area_s) <- "UTF-8"
                db$focus_area_s <- str_replace_all(db$focus_area_s, "and", ",")
                db <- separate_rows(db,focus_area_s,sep=",\\s+")


#Renaming categorgies to fit into the 9 already established 
                db$focus_area_s[ db$focus_area_s == "Girls" ] <- "Women & Girls"
                db$focus_area_s[ db$focus_area_s == "Girls " ] <- "Women & Girls"
                db$focus_area_s[ db$focus_area_s == "Women" ] <- "Women & Girls"
                db$focus_area_s[ db$focus_area_s == "Girld" ] <- "Women & Girls"
                db$focus_area_s[ db$focus_area_s == "Girld " ] <- "Women & Girls"
                db$focus_area_s[ db$focus_area_s == "Women " ] <- "Women & Girls"
                db$focus_area_s[ db$focus_area_s == "Women & Girls/ Mujeres y Ninas" ] <- "Women & Girls"
                db$focus_area_s[ db$focus_area_s == "Youth " ] <- "Youth & Children"
                db$focus_area_s[ db$focus_area_s == "Children" ] <- "Youth & Children"
                db$focus_area_s[ db$focus_area_s == "Youth & Childrens" ] <- "Youth & Children"
                db$focus_area_s[ db$focus_area_s == "Youth & Children/ Jovenes y Ninos" ] <- "Youth & Children"
                db$focus_area_s[ db$focus_area_s == "Youth & Children " ] <- "Youth & Children"
                db$focus_area_s[ db$focus_area_s == "Vocational Training" ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Inclusive Education" ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Education/ Educacion" ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Eduaction" ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Environment " ] <- "Environment & Conservation"
                db$focus_area_s[ db$focus_area_s == "Conservation" ] <- "Environment & Conservation"
                db$focus_area_s[ db$focus_area_s == "Environment & Conservation " ] <- "Environment & Conservation"
                db$focus_area_s[ db$focus_area_s == "Poverty Aliviation" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Poverty Alleviation" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Poverty Alleviation/ Mitigación de la Pobreza" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Poverty Alleviation " ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Special Needs" ] <- "Health"
                db$focus_area_s[ db$focus_area_s == "Reinseción de niños migrantes - Educación para el empleo" ] <- "Youth & Children"
                db$focus_area_s[ db$focus_area_s == "Vocational Training " ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Economic Development" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Misc: Housing" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Agriculture" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Coffee Farming" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Misc: Housing Security" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Misc: Orphanage" ] <- "Youth & Children"
                db$focus_area_s[ db$focus_area_s == "Nutrition" ] <- "Health"
                db$focus_area_s[ db$focus_area_s == "Misc: Special Needs" ] <- "Health"
                db$focus_area_s[ db$focus_area_s == "Construction" ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Construction " ] <- "Community Development"
                db$focus_area_s[ db$focus_area_s == "Culture" ] <- "Human Rights"
                db$focus_area_s[ db$focus_area_s == "Misc: orphan prevention program" ] <- "Youth & Children"
                db$focus_area_s[ db$focus_area_s == "Food Security" ] <- "Health"
                db$focus_area_s[ db$focus_area_s == "Permaculture" ] <- "Health"
                db$focus_area_s[ db$focus_area_s == "Misc: Elderly" ] <- "Health"
                db$focus_area_s[ db$focus_area_s == "Music" ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Music " ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Dance; Restorative Expressive Arts" ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Psychology " ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Critical-Thinking" ] <- "Education"
                db$focus_area_s[ db$focus_area_s == "Misc: Art" ] <- "Education"
                db <- rename(db, category = focus_area_s)


#Converting longitude and latitudes to coordinate type
# coordinates(db) <- c("longitude", "latitude") 
# proj4string(db) <- proj4string(db)


# Adding a column for NPO website 
                websites <- read.xlsx("ngo_database.xlsx", sheet = 8, startRow = 1, colNames =TRUE)
                websites <- clean_names(websites)
                websites$cause_name <- as.character(websites$cause_name)
                websites <- rename(websites, name = cause_name)
                websites$website <- as.character(websites$website)
                websites <- websites[,1:2]
                
                db <- left_join(db, websites, by = "name")
                db$website <- paste0("'<a href=\"",db$website,"\">",db$name,"</a>'")


#adding size, year founded, and budget information 
                more_info <- read.csv("Research Database - Sheet1.csv", stringsAsFactors = FALSE)
                more_info <- clean_names(more_info)
                db <- left_join(db, more_info, by = c("name"="npo"))
                db <- db %>% dplyr::select(name, category, address.x,region, latitude, longitude,
                                    website, year_founded, partner_status, size, budget) %>%
                             rename(address = address.x)
                db <- filter(db, size != "")
                db <- filter(db, year_founded != "")

#Creating the color palettes
#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#80BA5A,#E68310,#008695,#CF1C90,#f97b72,#4b4b8f,#A5AA99
                
                #Color for size 
                db <- mutate(db, sizecolor = NA)
                
                #miss spelling
                for (i in 1:length(db$size)){
                        if(db$size[i] == "Mico"){        
                                db$size[i] <- "Micro"               
                        }
                }
                
                
                for (i in 1:length(db$size)){
                        if(db$size[i] == "Small"){        
                                db$sizecolor[i] <- "#7F3C8D"               
                        }
                        else if(db$size[i] == "Medium"){        
                                db$sizecolor[i] <- "#11A579"              
                        }
                        else if(db$size[i] == "Large"){        
                                db$sizecolor[i] <- "#3969AC"              
                        }
                        else if(db$size[i] =="Micro"){        
                                db$sizecolor[i] <- "#F2B701"              
                        }
                        else if(db$size[i] == "Nano"){        
                                db$sizecolor[i] <- "#E73F74"  
                        }
                        else if(db$size[i] == "Very Large"){        
                                db$sizecolor[i] <- "#80BA5A"              
                        }
                        
                }
                
                
                #color for year founded. I used a scale. Importantly, it doesn't not work if there is an NA value
                #color palette for continous #f7feae,#045275
                db$year_founded <- as.numeric(db$year_founded)
                
                db <- filter(db, year_founded != "NA" )
                
                ii <- cut(db$year_founded, breaks = seq(min(db$year_founded), max(db$year_founded), len = 100), 
                          include.lowest = TRUE)
              
                db$year_founded_color <- colorRampPalette(c("#f7feae", "#045275"))(99)[ii]
                
                
                
                #color for partner status 
                for (i in 1:length(db$partner_status)){
                        if(db$partner_status[i] == "Y"){        
                                db$partnercolor[i] <- "#7F3C8D"               
                        }
                        else if(db$partner_status[i] == "D"){        
                                db$partnercolor[i] <- "#11A579"              
                        }
                        else if(db$partner_status[i] == "N"){        
                                db$partnercolor[i] <- "#3969AC"              
                        }
                }
                
                #adding a constant color 
                db$constant_color <- "#4b4b8f"
                db$constant <- 1
                
                
#Altering the size variable 
                db$budget <- str_replace_all(db$budget, "\\$", "")
                db$budget <- str_replace_all(db$budget, ",", "")
                db$budget <- as.numeric(db$budget)
                
                
                
#changing the order
                db <- select(db, name, category, address, region, latitude, longitude,website,year_founded, 
                             year_founded_color, partner_status, partnercolor, size, sizecolor, budget, constant, constant_color)
                
                db <- db %>% arrange(year_founded)
#Saving 
saveRDS(db, file="./data/guatemala_data.rds")



