# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(httr)
library(jsonlite)
library(scales)  # For formatting the y-axis

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Real Estate Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      textInput("location", "Enter Location:", value = "New York"),
      actionButton("search", "Search"),
      sliderInput("price_range", "Price Range:", min = 0, max = 1000000, value = c(100000, 500000)),
      sliderInput("bedrooms", "Number of Bedrooms:", min = 1, max = 10, value = 3),
      actionButton("compare", "Compare Neighborhoods")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Price Trends", plotlyOutput("priceTrends")),
        tabPanel("Neighborhood Comparison", plotlyOutput("neighborhoodComparison")),
        tabPanel("Investment Analysis", tableOutput("investmentAnalysis"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Generate data based on city
  getRealEstateData <- function(location) {
    set.seed(123)
    
    # Define base mean prices and standard deviations for different cities
    city_data <- list(
      "New York" = list(mean = 750000, sd = 50000),
      "San Francisco" = list(mean = 900000, sd = 60000),
      "Chicago" = list(mean = 500000, sd = 40000),
      "Miami" = list(mean = 600000, sd = 45000),
      "Austin" = list(mean = 400000, sd = 30000)
    )
    
    # Get city-specific data or default to New York
    city <- ifelse(location %in% names(city_data), location, "New York")
    mean_price <- city_data[[city]]$mean
    sd_price <- city_data[[city]]$sd
    
    neighborhoods <- c("Downtown", "Suburbs", "Midtown", "Uptown", "Outskirts")
    months <- seq.Date(Sys.Date() - 365, by = "month", length.out = 12)
    
    data <- expand.grid(
      Neighborhood = neighborhoods,
      Date = months
    )
    
    data <- data %>%
      mutate(
        Price = case_when(
          Neighborhood == "Downtown" ~ rnorm(n(), mean = mean_price, sd = sd_price),
          Neighborhood == "Suburbs" ~ rnorm(n(), mean = mean_price * 0.7, sd = sd_price * 0.8),
          Neighborhood == "Midtown" ~ rnorm(n(), mean = mean_price * 0.8, sd = sd_price * 0.9),
          Neighborhood == "Uptown" ~ rnorm(n(), mean = mean_price * 0.9, sd = sd_price * 0.85),
          Neighborhood == "Outskirts" ~ rnorm(n(), mean = mean_price * 0.5, sd = sd_price * 0.7)
        ),
        Bedrooms = sample(1:10, n(), replace = TRUE),
        Price = Price * (1 + 0.05 * sin(2 * pi * (as.numeric(format(Date, "%m")) / 12)))  # Add seasonality effect
      )
    
    return(data)
  }
  
  # Fetch and process data
  realEstateData <- eventReactive(input$search, {
    getRealEstateData(input$location)
  })
  
  # Render price trends plot
  output$priceTrends <- renderPlotly({
    data <- realEstateData()
    p <- ggplot(data, aes(x = Date, y = Price, color = Neighborhood)) +
      geom_line(size = 0.8) +
      labs(title = "Price Trends", x = "Date", y = "Price") +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +  # Format y-axis
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
      )
    ggplotly(p) %>% layout(margin = list(t = 80))
  })
  
  # Render neighborhood comparison plot
  output$neighborhoodComparison <- renderPlotly({
    data <- realEstateData() %>%
      filter(Price >= input$price_range[1], Price <= input$price_range[2],
             Bedrooms == input$bedrooms)
    p <- ggplot(data, aes(x = Neighborhood, y = Price, fill = Neighborhood)) +
      geom_boxplot() +
      labs(title = "Neighborhood Comparison", x = "Neighborhood", y = "Price") +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +  # Format y-axis
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "none"
      )
    ggplotly(p) %>% layout(margin = list(t = 80))
  })
  
  # Render investment analysis table
  output$investmentAnalysis <- renderTable({
    data <- realEstateData() %>%
      group_by(Neighborhood) %>%
      summarize(Average_Price = mean(Price),
                Median_Price = median(Price),
                Min_Price = min(Price),
                Max_Price = max(Price))
    data
  })
}

# Run the app
shinyApp(ui, server)
