#install.packages("censusapi")
#install.packages("tidycensus")
install.packages("mapview")
library("censusapi")
library("dplyr")
library("tidyr")
library("sf")
library("mapview")
library(ggmap)

#google map planned parenthood locations
source("phoods.R")
#key
Sys.setenv(CENSUS_KEY="d1382297b4b08cb4c0bf0e870ba7205a415b9f67")

Sys.getenv("CENSUS_KEY")

apis <- listCensusApis()
View(apis)

saipe_vars <- listCensusMetadata(
  name = "timeseries/poverty/saipe", 
  type = "variables")
head(saipe_vars)

pov_vars <- listCensusMetadata(
  name = "timeseries/poverty/histpov2",
  type = "variables"
)
head(pov_vars)

listCensusMetadata(
  name = "timeseries/poverty/histpov2",
  type = "geography"
)

listCensusMetadata(
  name = "timeseries/poverty/saipe", 
  type = "geography")

pov_national <- getCensus(
  name = "timeseries/poverty/histpov2",
  vars = c("PCTPOV","GEO_ID","POP"),
  region = "us:*",
  time = 2019)
View(pov_national)

#families in poverty by counties in michigan
saipe_county <- getCensus(
  name = "timeseries/poverty/saipe",
  vars = c("NAME","SAEMHI_UB90",
           "SAEPOVRTALL_LB90",
           "SAEPOVALL_UB90"),
           region = "county:*",
           regionin = "state:26"
           )
View(saipe_county)
#families under the poverty threshold in michigan
#<22,314 for a family of four in michigan
in_poverty <- filter(saipe_county,SAEMHI_UB90  <= 22314)

#rename columns
in_poverty <- rename(in_poverty,Income = SAEMHI_UB90, Median.Age = SAEPOVRTALL_LB90,
                     People.Count = SAEPOVALL_UB90)
View(in_poverty)

#taking counties in poverty and converting to latitude/longitude locations
#to search google maps api for planned parenthoods
county_coordinates <- mutate_geocode(in_poverty, NAME)
View(county_coordinates)

locations <- as_tibble(county_coordinates)
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
mapview(locations_sf)

#some of the coordinates are incorrect, manually change
county_coordinates[6,7] = -88.4903
county_coordinates[6,8] = 46.2758
county_coordinates[16,7] = -85.8486
county_coordinates[16,8] = 43.9447
county_coordinates[3,7] = -84.6897
county_coordinates[3,8] = 44.6524
county_coordinates[8,7] = -85.8486
county_coordinates[8,8] = 43.9447

#-84.68975 44.35161 looks like a good center
map <- get_googlemap(center = c(-86.68975,44.35161), zoom = 6)

ggmap(map) +
  geom_point(data = county_coordinates, aes(x = lon, y = lat, col = "blue")) +
  geom_point(data = lat_and_long, aes(x = lng, y = lat, col = "red"))
