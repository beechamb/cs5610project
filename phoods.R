#install.packages("googleway")
#install.packages("maps")
?ggmap::register_google
library(httr)
library(jsonlite)
library(dplyr)
library(ggrepel)
library(ggmap)
library(devtools)
library(googleway)
library(stringr)

source("census_proj.R")

register_google(key="AIzaSyBM0fQBiP-OVRxxtTJyM-_dNOBzstfzaxU")
key <- "AIzaSyBM0fQBiP-OVRxxtTJyM-_dNOBzstfzaxU"

phoods <- google_places(search_string = "reproductive health clinic",
              location = c(42.7325, -84.5555),
              key = key
              )
#phoods
#getting dataframe out of the returned list
#names(phoods)

results <- phoods$results
is.data.frame(results)
View(results)

names(results)

#converting latitude and longitude to corresponding county
library(sp)
library(maps)
library(maptools)

#need the dataframe to have only latitude and longitude to pass into the function
lat_and_long <- results$geometry$location
#View(lat_and_long)

lat_and_long2 <- lat_and_long[,c(2,1)]
#View(lat_and_long2)

#function to convert to county
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

counties <- latlong2county(lat_and_long2)
#View(counties) 

#in michigan exclusively
counties_in_mi <- str_detect(counties, "michigan")
counties_in_mi
