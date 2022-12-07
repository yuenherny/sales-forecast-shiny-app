# Load packages
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(stats)
library(forecast)

# ui
ui <- fluidPage(
  
  titlePanel("Sales Forecast App"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h5("Please ensure your uploaded CSV data (< 5MB) contains 
         `Year`, `Month` and `Sales Amt` columns."),
      
      h5("You can also use the app without uploading any data. 
         Just click the `Start forecast` button!"),
      
      fileInput(inputId = "input_file", label = "Upload CSV data here",
                  accept = c("text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
      actionButton(inputId = "run_button", label = "Start forecast")
      
    ),
    
    mainPanel(
      tableOutput(outputId = "table"),
      plotOutput(outputId = "plot"),
      textOutput(outputId = "mape"),
      verbatimTextOutput(outputId = "boxtest", placeholder = T),
      textOutput(outputId = "interpret_boxtest")
    )
    
  )
)

# server
server <- function(input, output) {
  
  # Get uploaded or default data
  get_file_or_default <- reactive({
    if (is.null(input$input_file)) {
      DATASET_PATH <- "dataset/Data.xlsx"
      readxl::read_xlsx(DATASET_PATH, sheet = "Data")
    } else {
      read.csv(input$input_file$datapath, check.names = F)
    }
  })
  
  # Function to preprocess df
  preprocess_df <- function(df){
    # Replace NaN with zero
    df$`Sales Amt`[is.na(df$`Sales Amt`)] = 0
    
    # Aggregate sales amount by year and month
    df <- df[c("Year", "Month", "Sales Amt")]
    df <- df %>% group_by(Year, Month) %>% 
      summarise(Monthly_Sales=sum(`Sales Amt`))
    df
  }
  
  # Function to get the years in df
  get_years <- function(df){
    # Get the years
    years <- unique(df$Year)
    
    # Split dataset into train and test
    start_train <- years[1]
    end_train <- tail(years, 2)[1]
    start_test <- tail(years, 2)[2]
    
    c(start_train, end_train, start_test)
  }
  
  # Function to compute Holt-Winters
  compute_holt_winter <- function(df, start_train, end_train, start_test){
    
    df_train <- subset(df, Year <= end_train)
    df_test <- subset(df, Year == start_test)

    # Perform Holt-Winters
    dfts_train <- df_train$Monthly_Sales %>%
      ts(start = c(start_train, 1), end = c(end_train, 12), frequency = 12)
  
    HoltWinters(dfts_train, seasonal = "multiplicative")
  }
  
  # Function to plot result
  plot_result <- function(df, hw_for, start_train, end_train, start_test){
    # Plot final result
    dfts_test <- df_test$Monthly_Sales %>% 
      ts(start = c(start_test, 1), end = c(start_test, 12), frequency = 12)
    
    dfts <- df$Monthly_Sales %>% 
      ts(start = c(start_train, 1), end = c(start_test, 12), frequency = 12)
    
    plot(hw_for, ylim = c(0,2.3e+7), ylab = "Monthly Sales")
    lines(dfts, lty = 2, col = "red")
    legend(x = "topright", legend=c("Forecast", "Fitted", "Original"), 
           col=c("blue", "black", "red"), lty=1:2, cex = 0.6)
  }
  
  get_head_df <- eventReactive(input$run_button, {
    head(get_file_or_default())
  })
  
  get_df <- eventReactive(input$run_button, {
    get_file_or_default()
  })
  
  eval_boxtest <- function(hw_for){
    Box.test(hw_for$residuals, type="Ljung-Box")
  }
  
  eval_mape <- function(hw, df, start_test){
    df_test <- subset(df, Year == start_test)
    hw.pred <- predict(hw, n.ahead = 12, 
                       prediction.interval = TRUE, level = 0.95)
    actual <- df_test$Monthly_Sales
    mean(abs((actual - hw.pred)/actual))*100
  }
  
  # Render df
  output$table <- renderTable(get_head_df())
  
  # Render plot
  output$plot <- renderPlot({
    df <- get_df()
    df <- preprocess_df(df)
    year_list <- get_years(df)
    hw <- compute_holt_winter(df, year_list[1], year_list[2], year_list[3])
    hw_for <- forecast(hw, h = 36, level = 0.95)
    
    plot_result(df, hw_for, year_list[1], year_list[2], year_list[3])
  })
  
  # Render MAPE
  output$mape <- renderText({
    df <- get_df()
    df <- preprocess_df(df)
    year_list <- get_years(df)
    hw <- compute_holt_winter(df, year_list[1], year_list[2], year_list[3])
    
    mape <- eval_mape(hw, df, year_list[3])
    paste("The mean absolute percentage error (MAPE) is", mape, ".")
  })
  
  # Render box test
  output$boxtest <- renderPrint({
    df <- get_df()
    df <- preprocess_df(df)
    year_list <- get_years(df)
    hw <- compute_holt_winter(df, year_list[1], year_list[2], year_list[3])
    hw_for <- forecast(hw, h = 36, level = 0.95)
    
    eval_boxtest(hw_for)
  })
  
  # Render box test interpretation
  output$interpret_boxtest <- renderText({
    "The Ljung-Box test checks if the residual is independent 
    (residual is supposed to be independent). 
    Residual is independent or random if p-value > 0.05, i.e.
    we fail to reject the null hypothesis."
  })
}

# shiny object
shinyApp(ui = ui, server = server)