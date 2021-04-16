library("mapview")
library("ggmap")
library("geosphere")
library("maps")
library("mapdata")
library("censusapi")
library("dplyr")
library("tidyr")
library("httr")
library("jsonlite")
library("dplyr")
library("ggmap")
library("devtools")
library("googleway")
library("stringr")
library("shiny")
library("leaflet")

#registering API keys and setting R environment
#google maps
source("keys.R")

register_google(key=gkey)

#census bureau
Sys.setenv(CENSUS_KEY=ckey)
Sys.getenv("CENSUS_KEY")

#viewing census APIs
apis <- listCensusApis()


#selecting appropriate variables from the API
variables <- listCensusMetadata(
  name = "timeseries/poverty/saipe",
  time = 2019
)


#selecting median income data in Michigan by county
data <- getCensus(
  name = "timeseries/poverty/saipe",
  time = 2019,
  vars = c("NAME","SAEMHI_PT",
           "SAEPOVALL_UB90"),
  region = "county:*",
  regionin = "state:26"
)


#renaming variables
income_stats <- rename(data, household.income = SAEMHI_PT,
                       people.count = SAEPOVALL_UB90)

#adding "michigan" to county strings in order to get correct coordinates
income_stats$NAME <- paste0(income_stats$NAME, ", Michigan")

#finding quartiles for thresholds
summary(income_stats$household.income)

#filtering families in first quartile
first.quartile <- filter(income_stats, household.income  <= 47482)

#filtering families in third quartile
third.quartile <- filter(income_stats, household.income >= 57626)


#converting counties both above and below median income to coordinates
#by use of the mutate geocode function
lower_county_coordinates <- mutate_geocode(first.quartile, NAME)
upper_county_coordinates <- mutate_geocode(third.quartile, NAME)

#reading in location data for familiy planning facilities
#taken from https://www.michigan.gov/mdhhs/0,5885,7-339-73971_4911_4912_6216_75529---,00.html
clinics <- read.csv("clinics.csv")
clinics <- select(clinics, County, Address)

#counting number of clinics per county
clinics.counts <- clinics %>%
  group_by(County) %>%
  mutate(count=n())


#convert title x addresses to coordinates using google maps api
for(i in 1:nrow(clinics)){
  result <- geocode(clinics$Address[i], output = "latlona", source = "google")
  clinics$lon[i] <- as.numeric(result[1])
  clinics$lat[i] <- as.numeric(result[2])
}

#getting map data for states and counties using maps and mapsdata packages
states <- map_data("state")
counties <- map_data("county")

#setting Michigan as a base map
mi_df <- subset(states, region == "michigan")
mi_county <- subset(counties, region == "michigan")

mi_base <- ggplot(data = mi_df, mapping = aes(x=long, y=lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray") +
  geom_polygon(data = mi_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)

mi_base +
  geom_point(data = impoverished_county_coordinates, mapping = aes(x=lon, y=lat, col = "Lower Income"), inherit.aes = FALSE) +
  geom_point(data = notimpoverished_county_coordinates, mapping = aes(x=lon, y=lat, col = "Higher Income"), inherit.aes = FALSE) +
  geom_point(data = clinics, mapping = aes(x=lon, y=lat, col = "Clinic"), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-90,-80), ylim =c(40,50)) +
  labs(color = "Legend", x = "Longitude", y = "Latitude")

#column telling whether or not county is below median income, where 1 = below and 0 = above
income_stats$first.quartile <- ifelse(income_stats$household.income <= 47485, 1, 0)

#column telling whether or not a county is not impoverished where 1 = not impoverished and 0 = impoverished
income_stats$third.quartile <- ifelse(income_stats$household.income > 57626, 1, 0)

#removing "michigan" from county name in income_stats in order to match with
#clinic string locations
income_stats$NAME <- str_remove(income_stats$NAME, ", Michigan")

#add column telling whether or not the county has a family planning clinic, where 1 = there is a clinic and 0 = no
income_stats$repo.healthcare <- ifelse(income_stats$NAME %in% clinics$County, 1, 0)


#chi square test between income and presence of a health facility
income.table <- table(income_stats$repo.healthcare, income_stats$household.income)
chisq.test(income.table, correct = F)

#chi square test between number of people in poverty and presence of a health facility
pov.table <-table(income_stats$repo.healthcare, income_stats$people.count)
chisq.test(pov.table, correct = F)

#correlation
cor(income_stats$repo.healthcare, income_stats$household.income)
cor(income_stats$repo.healthcare, income_stats$people.count)

#point biserial correlation
cor.test(income_stats$repo.healthcare, income_stats$household.income)


#graph showing the median income per county, colored by whether or not there
#is a family planning clinic
ggplot(income_stats, aes(x=NAME, y=household.income, color = as.factor(repo.healthcare))) +
  geom_point() +
  labs(x = "County", y = "Household Income", title = "Median County Income")

#logistic regression
logit <- glm(repo.healthcare ~ household.income + people.count, data = income_stats)
summary(logit)

#shiny application
my_ui <- fluidPage(
  #Application title
  titlePanel("Title X Clinics in Michigan"),
  #Dropdown menu for selecting county
  selectInput(
    inputId = "County",
    label = "Select County",
    choices = income_stats$NAME
  ),
  
  #graph
  mainPanel(
    plotOutput("map"),
    textOutput("message")
  )
)

my_server <- function(input, output) {

  newdata <- reactive({
    filter(clinics, County == input$County)
  })
  output$map <- renderPlot({
    mi_base +
      geom_point(newdata(), mapping = aes(x=lon, y=lat, col = "Clinic"), inherit.aes = FALSE) +
      coord_cartesian(xlim = c(-90,-82), ylim =c(40,48)) +
      labs(color = "Legend", x = "Longitude", y = "Latitude")
  })
}

shinyApp(ui = my_ui, server = my_server)



