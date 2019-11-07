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

# read the shape file using sf package
guatemala.shape_orig <- st_read("Guatemala_shape_files/GTM_adm1.shp", stringsAsFactors=FALSE)

# transform it to WGS84 coordinate system
Guatemala <- st_transform(guatemala.shape_orig, "+proj=longlat +ellps=WGS84 +datum=WGS84")


#calling in the excel file 
db <- read.xlsx("ngo_database.xlsx", sheet = 3, startRow = 1, colNames =TRUE)
db <- clean_names(db)
db$name <- as.character(db$name)
db$focus_area_s <- as.character(db$focus_area_s)
Encoding(db$name) <- "UTF-8"
Encoding(db$focus_area_s) <- "UTF-8"
db$focus_area_s <- str_replace_all(db$focus_area_s, "and", ",")
db <- separate_rows(db,focus_area_s,sep=",\\s+")
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




#Converting longitude and latitudes to coordinate type
coordinates(db) <- c("longitude", "latitude") 
proj4string(db) <- proj4string(db)

#plotting 
mapview(Guatemala, color = "cyan", col.regions = "white") + mapview(db)
