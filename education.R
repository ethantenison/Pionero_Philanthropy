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
db <- db[1:69,]
db <- separate_rows(db,focus_area_s,sep=",\\s+")




#Converting longitude and latitudes to coordinate type
coordinates(db) <- c("longitude", "latitude") 
proj4string(db) <- proj4string(db)

#plotting 
mapview(Guatemala, color = "cyan", col.regions = "white") + mapview(db)
