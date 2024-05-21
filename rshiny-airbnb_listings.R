# Downloading libraries:

library(ggplot2)
library(dplyr)
library(leaflet)
library(shiny)
library(lubridate)
library(textdata)
library(tidytext)
library(tidyr)
library(sf)
library(mapview)
library(DT)

# Uploading dataset:
airbnb_data <- read.csv('AB_NYC_2019.csv')

# Data cleaning: Remove rows with missing values
airbnb_data <- na.omit(airbnb_data)

# Filter out non-finite values in the price column
airbnb_data <- airbnb_data %>%
  filter(is.finite(price))

nyc_sf <- st_as_sf(airbnb_data, coords = c("longitude", "latitude"), crs = 4326)


# Define UI for the Shiny dashboard
ui <- fluidPage(
  titlePanel("NYC Airbnb Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("neighbourhood_group", "Neighborhood Group", 
                  choices = unique(airbnb_data$neighbourhood_group)),
      selectInput("room_type", "Room Type", 
                  choices = unique(airbnb_data$room_type)),
      sliderInput("price_range", "Price Range", 
                  min = min(airbnb_data$price), max = max(airbnb_data$price),
                  value = c(min(airbnb_data$price), max(airbnb_data$price)))
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("price_distribution"),
      plotOutput("time_series"),
      dataTableOutput("summary_table"),
      textOutput("summary_text")
    )
  )
)


# Define server logic for the Shiny dashboard
server <- function(input, output, session) {
  filtered_data <- reactive({
    airbnb_data %>% 
      filter(neighbourhood_group == input$neighbourhood_group,
             room_type == input$room_type,
             price >= input$price_range[1],
             price <= input$price_range[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude,
                       radius = ~ifelse(price < 50, 3, ifelse(price < 100, 5, 7)),
                       color = ~ifelse(neighbourhood_group == 'Manhattan', 'red', 
                                       ifelse(neighbourhood_group == 'Brooklyn', 'blue', 
                                              ifelse(neighbourhood_group == 'Queens', 'green', 
                                                     ifelse(neighbourhood_group == 'Bronx', 'purple', 'orange')))),
                       stroke = FALSE, fillOpacity = 0.5,
                       popup = ~paste("Price: $", price, "<br>",
                                      "Room Type: ", room_type, "<br>",
                                      "Neighborhood: ", neighbourhood))
  })
  
  output$price_distribution <- renderPlot({
    ggplot(filtered_data(), aes(x = price)) +
      geom_histogram(binwidth = 50, fill = 'grey', color = 'black') +
      xlim(0, 1000) +
      labs(title = paste('Price Distribution in', input$neighbourhood_group),
           x = 'Price', y = 'Frequency') +
      theme_minimal() + 
      theme(
        plot.title = element_text(size = 28, face = "bold", color = "black")
      )
    
  })
  
  output$time_series <- renderPlot({
    ggplot(filtered_data(), aes(x = as.Date(last_review))) +
      geom_line(stat = 'count', color = 'blue') +
      labs(title = paste('Number of Reviews Over Time in', input$neighbourhood_group),
           x = 'Date', y = 'Number of Reviews') +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 28, face = "bold", color = "black")
      )
    
  })
  
  output$summary_table <- renderDataTable({
    filtered_data() %>%
      select(name, host_name, neighbourhood, price, minimum_nights, number_of_reviews) %>%
      datatable()
  })
  
  output$summary_text <- renderText({
    data <- filtered_data()
    avg_price <- mean(data$price)
    total_listings <- nrow(data)
    total_reviews <- sum(data$number_of_reviews)
    paste("Average Price: $", round(avg_price, 2), 
          " | Total Listings: ", total_listings,
          " | Total Reviews: ", total_reviews)
  })
}

shinyApp(ui, server)
