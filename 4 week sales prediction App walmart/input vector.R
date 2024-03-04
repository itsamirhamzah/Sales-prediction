# Load the required libraries
library(RMariaDB)
library(shiny)
library(dplyr)
library(randomForest)

# UI definition
ui <- fluidPage(
  titlePanel("4-week Sales Prediction"),
  sidebarLayout(
    sidebarPanel(
      # Input features
      h3("Input Features"),
      textInput("temperature", "Temperature (comma-separated)", "55, 52, 49, 59"),
      textInput("fuel_price", "Fuel Price (comma-separated)", "4, 4, 4, 4"),
      textInput("cpi", "CPI (comma-separated)", "140, 142, 138, 136"),
      textInput("unemployment", "Unemployment (comma-separated)", "7.8, 7.9, 7.9, 8.0"),
      actionButton("submitbutton", "Submit", class = "btn btn-primary")
    ),
    mainPanel(
      h3("Status/Output"),
      verbatimTextOutput("contents"),
      tableOutput("tabledata")
    )
  )
)

# Server function
server <- function(input, output, session) {
  connect.mis410 <- dbConnect(RMariaDB::MariaDB(),
                              dbname = "mis410",
                              host = "localhost",
                              user = "root",
                              password = ""
  )
  
  salesprice <- dbReadTable(connect.mis410, "walmart")
  on.exit(dbDisconnect(connect.mis410), add = TRUE)
  df15 <- salesprice[salesprice$Store == 15, ]
  
  # Construct lagged features
  library(dplyr)
  df15$cpi_l1 <- dplyr::lag(df15$CPI, n = 1)
  df15$cpi_l2 <- dplyr::lag(df15$CPI, n = 2)
  df15$cpi_l3 <- dplyr::lag(df15$CPI, n = 3)
  df15$cpi_l4 <- dplyr::lag(df15$CPI, n = 4)
  
  df15$unemployment_l1 <- dplyr::lag(df15$Unemployment, n = 1)
  df15$unemployment_l2 <- dplyr::lag(df15$Unemployment, n = 2)
  df15$unemployment_l3 <- dplyr::lag(df15$Unemployment, n = 3)
  df15$unemployment_l4 <- dplyr::lag(df15$Unemployment, n = 4)
  
  df15$fuel_price_l1 <- dplyr::lag(df15$Fuel_Price, n = 1)
  df15$fuel_price_l2 <- dplyr::lag(df15$Fuel_Price, n = 2)
  df15$fuel_price_l3 <- dplyr::lag(df15$Fuel_Price, n = 3)
  df15$fuel_price_l4 <- dplyr::lag(df15$Fuel_Price, n = 4)
  
  
  df15$temperature_l1 <- dplyr::lag(df15$Temperature, n = 1)
  df15$temperature_l2 <- dplyr::lag(df15$Temperature, n = 2)
  df15$temperature_l3 <- dplyr::lag(df15$Temperature, n = 3)
  df15$temperature_l4 <- dplyr::lag(df15$Temperature, n = 4)
  
  # Clean data
  salesprice.clean <- na.omit(df15)
  
  # Fit a machine learning model
  rf_classifier <- randomForest(Weekly_Sales ~  
                                  temperature_l1 + temperature_l2 + temperature_l3 + temperature_l4 + 
                                  fuel_price_l1 + fuel_price_l2 + fuel_price_l3 + fuel_price_l4 + 
                                  cpi_l1 + cpi_l2 + cpi_l3 + cpi_l4 + 
                                  unemployment_l1 + unemployment_l2 + unemployment_l3 + unemployment_l4, 
                                data = salesprice.clean, ntree = 300, importance = TRUE)
  
  trainData <- salesprice.clean[1:(nrow(salesprice.clean) - 4), ] 
  testData <- salesprice.clean[(nrow(salesprice.clean) - 4 + 1):nrow(salesprice.clean), ] # new data
  
  # Define a reactive dataset input function
  datasetInput <- reactive({
    # Create data frames from the vector inputs
    temperature_values <- as.numeric(unlist(strsplit(input$temperature, ",")))
    fuel_price_values <- as.numeric(unlist(strsplit(input$fuel_price, ",")))
    cpi_values <- as.numeric(unlist(strsplit(input$cpi, ",")))
    unemployment_values <- as.numeric(unlist(strsplit(input$unemployment, ",")))
    
    # Create a data frame with the lagged features from the input
    df <- data.frame(
      temperature_l1 = temperature_values[1],
      temperature_l2 = temperature_values[2],
      temperature_l3 = temperature_values[3],
      temperature_l4 = temperature_values[4],
      fuel_price_l1 = fuel_price_values[1],
      fuel_price_l2 = fuel_price_values[2],
      fuel_price_l3 = fuel_price_values[3],
      fuel_price_l4 = fuel_price_values[4],
      cpi_l1 = cpi_values[1],
      cpi_l2 = cpi_values[2],
      cpi_l3 = cpi_values[3],
      cpi_l4 = cpi_values[4],
      unemployment_l1 = unemployment_values[1],
      unemployment_l2 = unemployment_values[2],
      unemployment_l3 = unemployment_values[3],
      unemployment_l4 = unemployment_values[4]
    )
    
    # Make a prediction
    predictions <- predict(rf_classifier, newdata = df, interval = "prediction")
    
    # Return the prediction as a data frame
    output_data <- data.frame(Prediction = predictions)
    output_data
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      "Prediction is complete."
    } else {
      "Server is ready!"
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      datasetInput()
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

