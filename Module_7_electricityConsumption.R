# Module 7 _ Electricity consumption
# https://www.geeksforgeeks.org/r-language/home-energy-usage-monitoring-dashboard-in-r/
# Load necessary libraries
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(ggplot2)

data <- read.csv("/Users/lakshmi/Desktop/MyPersonal/DataSets/household_power_consumption.csv")
# Convert Date and Time to datetime format
data <- data %>%
  mutate(DateTime = dmy_hms(paste(Date, Time)),
         Global_active_power = as.numeric(Global_active_power),
         Global_reactive_power = as.numeric(Global_reactive_power),
         Voltage = as.numeric(Voltage),
         Global_intensity = as.numeric(Global_intensity),
         Sub_metering_1 = as.numeric(Sub_metering_1),
         Sub_metering_2 = as.numeric(Sub_metering_2),
         Sub_metering_3 = as.numeric(Sub_metering_3)) %>%
  filter(!is.na(Global_active_power)) 

head(data)

#-----------------------------------------------------------------------------
#   Preprocessing the Data
#-----------------------------------------------------------------------------

data <- data %>%
  filter(!is.na(Global_active_power)) 

# Example: Log transformation
data <- data %>%
  mutate(Log_Global_active_power = log(Global_active_power + 1)) 

# Example: Aggregating to daily energy consumption
daily_energy <- data %>%
  mutate(Date = as_date(DateTime)) %>%
  group_by(Date) %>%
  summarise(Daily_Consumption_kWh = sum(Global_active_power, na.rm = TRUE))

#-----------------------------------------------------------------------------
#   Time Series Analysis
#-----------------------------------------------------------------------------

# Aggregate data to daily energy consumption
daily_energy <- data %>%
  mutate(Date = as_date(DateTime)) %>%
  group_by(Date) %>%
  summarise(Daily_Consumption_kWh = sum(Global_active_power, na.rm = TRUE))

# Plot daily energy consumption
ggplot(daily_energy, aes(x = Date, y = Daily_Consumption_kWh)) +
  geom_line(color = "plum3") +
  labs(title = "Daily Energy Consumption", x = "Date", y = "Energy Consumption (kWh)") +
  theme_minimal()

#-----------------------------------------------------------------------------
#  Energy Consumption Trends Dashboard
#-----------------------------------------------------------------------------
# Load necessary libraries
install.packages("shiny")
install.packages("plotly")

library(shiny)
library(plotly)
# Assuming 'data' is already loaded and cleaned

# Define UI for application
ui <- fluidPage(
  titlePanel("Energy Consumption Trends Dashboard"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange", "Date range:", 
                     start = min(data$DateTime), 
                     end = max(data$DateTime),
                     format = "yyyy-mm-dd", separator = " - ")
    ),
    mainPanel(
      plotlyOutput("lineChart"),
      plotOutput("barGraph"),
      plotlyOutput("heatmap")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on date range input
  filtered_data <- reactive({
    data %>%
      filter(DateTime >= input$daterange[1] & DateTime <= input$daterange[2])
  })
  
  # Aggregate data to daily energy consumption
  daily_energy <- reactive({
    filtered_data() %>%
      mutate(Date = as.Date(DateTime)) %>%
      group_by(Date) %>%
      summarise(Daily_Consumption_kWh = sum(Global_active_power, na.rm = TRUE))
  })
  
  # Plot daily energy consumption (Line chart)
  output$lineChart <- renderPlotly({
    plot_ly(daily_energy(), x = ~Date, y = ~Daily_Consumption_kWh, type = 'scatter', 
            mode = 'lines', line = list(color = "violetred2", width = 3)) %>%
      layout(title = "Daily Energy Consumption", xaxis = list(title = "Date"), 
             yaxis = list(title = "Energy Consumption (kWh)"))
  })
  
  # Aggregate data to hourly energy consumption
  hourly_energy <- reactive({
    filtered_data() %>%
      mutate(Hour = hour(DateTime)) %>%
      group_by(Hour) %>%
      summarise(Hourly_Consumption_kWh = sum(Global_active_power, na.rm = TRUE))
  })
  
  # Plot hourly energy consumption (Bar graph)
  output$barGraph <- renderPlot({
    ggplot(hourly_energy(), aes(x = factor(Hour), y = Hourly_Consumption_kWh)) +
      geom_bar(stat = "identity", fill = "mediumpurple3") +
      labs(title = "Hourly Energy Consumption", x = "Hour of Day", 
           y = "Energy Consumption (kWh)") +
      theme_minimal()
  })
  
  # Create heatmap for energy consumption density
  output$heatmap <- renderPlotly({
    # Aggregate data to create heatmap
    heatmap_data <- filtered_data() %>%
      mutate(Date = as.Date(DateTime), Hour = hour(DateTime)) %>%
      group_by(Date, Hour) %>%
      summarise(Avg_Active_Power = mean(Global_active_power, na.rm = TRUE)) %>%
      ungroup() %>%
      complete(Date, nesting(Hour), fill = list(Avg_Active_Power = 0))
    
    plot_ly(heatmap_data, x = ~Hour, y = ~Date, z = ~Avg_Active_Power, 
            type = "heatmap") %>%
      layout(title = "Energy Consumption Heatmap", xaxis = list(title = "Hour of Day"), 
             yaxis = list(title = "Date"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
