library("censusapi")
library("dplyr")
library("tidyr")
#library("sf")
library("mapview")
library("ggmap")
library("geosphere")
library(maps)
library(mapdata)
#library("rgeos")
#library("spectralGP")
#library("gdistance")
#library("fossil")
library(httr)
library(jsonlite)
library(dplyr)
library(ggmap)
library(devtools)
library(googleway)
library(stringr)

#google api key
register_google(key="AIzaSyBM0fQBiP-OVRxxtTJyM-_dNOBzstfzaxU")
key <- "AIzaSyBM0fQBiP-OVRxxtTJyM-_dNOBzstfzaxU"

#census key
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
View(pov_vars)

#listCensusMetadata(
#  name = "timeseries/poverty/histpov2",
#  type = "geography"
#)

#listCensusMetadata(
#  name = "timeseries/poverty/saipe", 
#  type = "geography")

#pov_national <- getCensus(
#  name = "timeseries/poverty/histpov2",
#  vars = c("PCTPOV","GEO_ID","POP"),
#  region = "us:*",
#  time = 2019)
#View(pov_national)

#families in poverty by counties in michigan
saipe_county <- getCensus(
  name = "timeseries/poverty/saipe",
  vars = c("NAME","SAEMHI_UB90",
           "SAEPOVALL_UB90"),
           region = "county:*",
           regionin = "state:26"
           )
View(saipe_county)


#rename columns
income_stats <- rename(saipe_county, Income = SAEMHI_UB90,
                       People.Count = SAEPOVALL_UB90) %>% drop_na()
View(income_stats)


#family of four in Michigan middle-class is 60088 to 179368
affluent <- filter(income_stats, Income >= 60008)


#families under the poverty threshold in michigan
#<22,314 for a family of four in michigan
in_poverty <- filter(income_stats, Income  <= 26500)

#taking counties in poverty and converting to latitude/longitude locations
#to search google maps api for planned parenthoods
poor_county_coordinates <- mutate_geocode(in_poverty, NAME)


affluent_county_coordinates <- mutate_geocode(affluent, NAME)
locations2 <- as_tibble(affluent_county_coordinates)
locations_sf2 <- st_as_sf(locations2, coords = c ("lon","lat"), crs = 4326)
mapview(locations_sf2)

locations <- as_tibble(county_coordinates)
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
mapview(locations_sf)

#counties with title x family planning clinics
clinics <- read.csv("clinics.csv")
View(clinics)

#convert title x addresses to coordinates using google maps api
for(i in 1:nrow(clinics)){
  result <- geocode(clinics$Address[i], output = "latlona", source = "google")
  clinics$lon[i] <- as.numeric(result[1])
  clinics$lat[i] <- as.numeric(result[2])
}

#healthlocations <- as_tibble(county_coords)
#healthlocations_sf <- st_as_sf(healthlocations, coords = c("lon","lat"), crs = 4326)
#mapview(healthlocations_sf)
#View(county_coords)

#some of the coordinates are incorrect, manually change
poor_county_coordinates[6,7] = -88.4903
poor_county_coordinates[6,8] = 46.2758
poor_county_coordinates[16,7] = -85.8486
poor_county_coordinates[16,8] = 43.9447
poor_county_coordinates[3,7] = -84.6897
poor_county_coordinates[3,8] = 44.6524
poor_county_coordinates[8,7] = -85.8486
poor_county_coordinates[8,8] = 43.9447

affluent_county_coordinates[102,7] = -86.0122
affluent_county_coordinates[102,8] = 41.9299
affluent_county_coordinates[135,7] = -83.5070
affluent_county_coordinates[135,8] = 41.9739


#getting map data for states and counties
states <- map_data("state")
counties <- map_data("county")

#setting michigan as a base
mi_df <- subset(states, region == "michigan")
head(mi_df)
mi_county <- subset(counties, region == "michigan")
head(mi_county)

mi_base <- ggplot(data = mi_df, mapping = aes(x=long, y=lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray") +
  geom_polygon(data = mi_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)
  
mi_base +
  geom_point(data = poor_county_coordinates, mapping = aes(x=lon, y=lat, col = "Poor"), inherit.aes = FALSE) +
  geom_point(data = affluent_county_coordinates, mapping = aes(x=lon, y=lat, col = "Loaded"), inherit.aes = FALSE) +
  geom_point(data = clinics, mapping = aes(x=lon, y=lat, col = "Clinic"), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-90,-80)) +
  labs(color = "Legend")
  

#-84.68975 44.35161 looks like a good center
map <- get_googlemap(center = c(-86.68975,44.35161), zoom = 6)

ggmap(map) +
  geom_point(data = poor_county_coordinates, mapping = aes(x=lon,
                                                           y=lat, col="Poor")) +
  geom_point(data = clinics, mapping = aes(x=lon,y=lat,col="Clinic"), position = "jitter") +
  geom_point(data = affluent_county_coordinates, mapping = aes(x=lon,y=lat,col = "Loaded"), position = "jitter")

#add column telling whether or not the county has a family planning clinic, where 1 = there is a clinic and 0 = no
income_stats$repo.healthcare <- ifelse(income_stats$NAME %in% clinics$County, 1, 0)
#View(income_stats)

#column telling whether or not county is impoverished, where 1 = impoverished and 0 = not impoverished
income_stats$in.poverty <- ifelse(income_stats$Income <= 26500, 1, 0)
#View(income_stats)

#column telling whether or not county is rich af, where 1 = rich and 0 = not rich
income_stats$is.affluent <- ifelse(income_stats$Income >= 60088, 1, 0)
View(income_stats)

#chi square test poverty
pov_table <- table(income_stats$in.poverty, income_stats$repo.healthcare)
pov_table
chisq.test(pov_table, correct = F)

#chi square test affluent
aff_table <- table(income_stats$is.affluent, income_stats$repo.healthcare)
chisq.test(aff_table, correct = F)

#correlation
cor(income_stats$repo.healthcare, income_stats$Income)
cor(income_stats$repo.healthcare, income_stats$in.poverty)
cor(income_stats$repo.healthcare, income_stats$is.affluent)

#graph of clinics per affluent county vs clinics per impoverished county
clinic_counts <- income_stats %>%
  select(repo.healthcare, is.affluent, in.poverty) %>%
  gather(key = repo.healthcare,
         value = "yes") %>%
  group_by(repo.healthcare) %>%
  summarize(count = sum(yes==1))
ggplot(clinic_counts,aes(x=repo.healthcare, y=count)) +
  geom_bar(stat = "identity")

#graph of mean income for affluent counties and impoverished counties
#which have a title x clinic
income_medians <- income_stats %>%
  select(repo.healthcare, is.affluent, in.poverty, Income) %>%
  group_by(repo.healthcare) %>%
  summarize(median_income = mean(Income))

ggplot(data = income_medians) + 
  geom_bar(mapping=aes(x=repo.healthcare, y=median_income))
  
#logistic regression, week 11 cs5610
#add number of clinics to counties dataframe