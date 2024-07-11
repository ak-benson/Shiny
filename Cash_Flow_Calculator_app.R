# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(plotly)
library(shinythemes)
library(tidyr)
library(RColorBrewer)  # For improved color palette

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Apply a theme
  titlePanel("Monthly Cash Flow and Savings Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:", choices = month.name, selected = "January"),
      
      # Income Inputs
      numericInput("salary", "Salary:", 0, min = 0),
      numericInput("investments", "Investments:", 0, min = 0),
      numericInput("other_income", "Other Income:", 0, min = 0),
      
      # Expense Inputs
      numericInput("housing", "Housing:", 0, min = 0),
      numericInput("utilities", "Utilities:", 0, min = 0),
      numericInput("transportation", "Transportation:", 0, min = 0),
      numericInput("food", "Food:", 0, min = 0),
      numericInput("entertainment", "Entertainment:", 0, min = 0),
      numericInput("miscellaneous", "Miscellaneous:", 0, min = 0),
      
      # Savings Input
      numericInput("initial_savings", "Initial Savings:", 0, min = 0),
      
      actionButton("add", "Add Data"),
      actionButton("reset", "Reset Data"),
      fileInput("file", "Upload CSV", accept = c(".csv")),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", tableOutput("dataTable")),
        tabPanel("Cash Flow Graph", plotlyOutput("cashFlowPlot")),
        tabPanel("Savings Graph", plotlyOutput("savingsPlot"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Initialize empty data frame
  data <- reactiveVal(data.frame(
    Month = character(),
    Salary = numeric(),
    Investments = numeric(),
    Other_Income = numeric(),
    Housing = numeric(),
    Utilities = numeric(),
    Transportation = numeric(),
    Food = numeric(),
    Entertainment = numeric(),
    Miscellaneous = numeric(),
    Savings = numeric(),
    Cash_Flow = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Add data for current month
  observeEvent(input$add, {
    total_income <- input$salary + input$investments + input$other_income
    total_expenses <- input$housing + input$utilities + input$transportation +
      input$food + input$entertainment + input$miscellaneous
    cash_flow <- total_income - total_expenses
    
    if (nrow(data()) == 0) {
      cumulative_savings <- input$initial_savings + cash_flow
    } else {
      cumulative_savings <- data()$Savings[nrow(data())] + cash_flow
    }
    
    dataNew <- data.frame(
      Month = input$month,
      Salary = input$salary,
      Investments = input$investments,
      Other_Income = input$other_income,
      Housing = -input$housing,  # Make expenses negative
      Utilities = -input$utilities,
      Transportation = -input$transportation,
      Food = -input$food,
      Entertainment = -input$entertainment,
      Miscellaneous = -input$miscellaneous,
      Savings = cumulative_savings,
      Cash_Flow = cash_flow,
      stringsAsFactors = FALSE
    )
    data(data() %>% bind_rows(dataNew))
  })
  
  # Reset data frame
  observeEvent(input$reset, {
    data(data.frame(
      Month = character(),
      Salary = numeric(),
      Investments = numeric(),
      Other_Income = numeric(),
      Housing = numeric(),
      Utilities = numeric(),
      Transportation = numeric(),
      Food = numeric(),
      Entertainment = numeric(),
      Miscellaneous = numeric(),
      Savings = numeric(),
      Cash_Flow = numeric(),
      stringsAsFactors = FALSE
    ))
  })
  
  # Upload data from CSV
  observeEvent(input$file, {
    req(input$file)
    uploadedData <- read.csv(input$file$datapath)
    data(uploadedData)
  })
  
  # Calculate monthly totals by category and month
  dataByMonth <- reactive({
    data() %>%
      group_by(Month) %>%
      summarize(
        Salary = sum(Salary),
        Investments = sum(Investments),
        Other_Income = sum(Other_Income),
        Housing = sum(Housing),
        Utilities = sum(Utilities),
        Transportation = sum(Transportation),
        Food = sum(Food),
        Entertainment = sum(Entertainment),
        Miscellaneous = sum(Miscellaneous),
        Savings = max(Savings),
        Cash_Flow = sum(Cash_Flow)
      )
  })
  
  # Render output table
  output$dataTable <- renderTable({
    dataByMonth()
  }, rownames = FALSE)
  
  # Render cash flow graph
  output$cashFlowPlot <- renderPlotly({
    plotData <- dataByMonth() %>%
      pivot_longer(cols = c(Salary, Investments, Other_Income, Housing, Utilities, 
                            Transportation, Food, Entertainment, Miscellaneous, Cash_Flow),
                   names_to = "Category", values_to = "Amount")
    
    # Convert Month to factor to ensure chronological order
    plotData$Month <- factor(plotData$Month, levels = month.name)
    
    # Define color palette
    colors <- brewer.pal(n = 10, name = "Set3")
    
    p <- ggplot(plotData, aes(x = Month, y = Amount, fill = Category)) +
      geom_col() +  # Stacked bar plot
      scale_fill_manual(values = colors, name = "Category") +
      labs(title = "Monthly Cash Flow by Category", x = "Month", y = "Amount") +
      theme_bw()
    
    ggplotly(p)
  })
  
  # Render savings graph
  output$savingsPlot <- renderPlotly({
    savingsData <- data() %>%
      select(Month, Savings) %>%
      distinct() %>%
      arrange(match(Month, month.name))
    
    # Convert Month to factor to ensure chronological order
    savingsData$Month <- factor(savingsData$Month, levels = month.name)
    
    p <- ggplot(savingsData, aes(x = Month, y = Savings, group = 1)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      labs(title = "Accumulated Savings Over Time", x = "Month", y = "Savings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
