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
library(xlsx)
library(janitor)

# read the shape file using sf package
guatemala.shape_orig <- st_read("Guatemala_shape_files/GTM_adm1.shp", stringsAsFactors=FALSE)

# transform it to WGS84 coordinate system
Guatemala <- st_transform(guatemala.shape_orig, "+proj=longlat +ellps=WGS84 +datum=WGS84")


#calling in the excel file 
edu <- read.xlsx("educationattempt.xlsx", "OFFICIAL NPO LIST", header=TRUE)
edu <- clean_names(edu)
edu$name <- as.character(edu$name)
edu$focus_area_s <- as.character(edu$focus_area_s)
edu <- rename(edu, longitude = logitude)
Encoding(edu$name) <- "UTF-8"
Encoding(edu$focus_area_s) <- "UTF-8"
edu <- edu[1:6,]




#Converting longitude and latitudes to coordinate type
coordinates(edu) <- c("longitude", "latitude") 
proj4string(edu) <- proj4string(edu)

#plotting 
mapview(Guatemala, color = "cyan", col.regions = "white") + mapview(edu)
