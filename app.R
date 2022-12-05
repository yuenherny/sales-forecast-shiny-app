# Load packages
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(stats)
library(forecast)

# Load data
DATASET_PATH <- "dataset/Data.xlsx"
df <- readxl::read_xlsx(DATASET_PATH, sheet = "Data")

# Replace NaN with zero
df$`Sales Amt`[is.na(df$`Sales Amt`)] = 0

# Aggregate sales amount by year and month
df <- df[c("Year", "Month", "Sales Amt")]
df <- df %>% group_by(Year, Month) %>% summarise(Monthly_Sales=sum(`Sales Amt`))

# Split dataset into train and test
df_train <- subset(df, Year <= 2020)
df_test <- subset(df, Year == 2021)

# Perform Holt-Winters
dfts_train <- df_train$Monthly_Sales %>% ts(start = c(2019, 1), end = c(2020, 12), frequency = 12)

hw <- HoltWinters(dfts_train, seasonal = "multiplicative")
hw.pred <- predict(hw, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

hw_for <- forecast(hw, h = 12, level = 0.95)

# Plot final result
dfts_test <- df_test$Monthly_Sales %>% ts(start = c(2021, 1), end = c(2021, 12), frequency = 12)

hw_for <- forecast(hw, h = 36, level = 0.95)
plot(hw_for, ylim = c(0,2.3e+7))
lines(dfts, lty = 2, col = "red")
legend(x = "topright", legend=c("Forecast", "Fitted", "Original"), col=c("blue", "black", "red"), lty=1:2, cex = 0.6)

# ui
ui <- fluidPage(
  
)

# server
server <- function(input, output, session) {
  
}

# shiny object
shinyApp(ui = ui, server = server)