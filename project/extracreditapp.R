#setting source
source("semester_project.R")

#shiny application
my_ui <- fluidPage(
  #Application title
  titlePanel("Title X Clinics and Income in Michigan"),
  
  #sidebar layout
  sidebarLayout(
    
    #dropdown menu for selecting county
    selectInput(
      inputId = "County",
      label = "Select County",
      choices = income_stats$NAME
    ),

  #main panel for displaying outputs
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Locations", plotOutput("map")),
                tabPanel("Income", plotOutput("income")),
                tabPanel("Population", plotOutput("population")))
  )
))

my_server <- function(input, output) {
  
  #creating a reactive dataframe
  newdata <- reactive({
    filter(clinics, County == input$County)
  })
  
  newdata2 <- reactive({
    filter(income_stats, NAME == input$County)
  })
  
  #plot of locations in selected county
  output$map <- renderPlot({
    mi_base +
      geom_point(newdata(), mapping = aes(x=lon, y=lat, col = "Clinic"), size = 5, inherit.aes = FALSE) +
      coord_cartesian(xlim = c(-90,-81), ylim =c(40,48)) +
      labs(color = "Legend", x = "Longitude", y = "Latitude", title = "Locations of Title X Clinics in your county")
  })
  
  #income of selected county
  output$income <- renderPlot({
    ggplot(newdata2(), mapping = aes(x=NAME, y=household.income)) +
    geom_bar(stat = "identity", fill = "purple") +
      labs(x = "County", y = "Median Income", title = "Income of Selected County") 
  })
  
  #population of those in poverty in the county graph
  output$population <- renderPlot({
    ggplot(newdata2(), mapping = aes(x=NAME, y=people.count)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(x = "County", y = "People in Poverty", title = "Number of People in Poverty
           of Selected County")
  })
  #address message of selected county

#  output$address <- if(nrow(newdata()) >= 1){renderText({
#    message <- "The addresses of Title X Locations in your selected county are: "
#    message_str <- paste0(message, input$Address)})}
#    else{
#      message <- "There are no clinics in your selected county"
#    }
#    
}

shinyApp(ui = my_ui, server = my_server)
