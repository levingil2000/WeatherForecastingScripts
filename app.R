library(shiny)
library(tidyverse)
library(lubridate)

#Converter
format_hour_ampm <- function(hour) {
  ifelse(hour == 0, "12 AM",
         ifelse(hour < 12, paste0(hour, " AM"),
                ifelse(hour == 12, "12 PM",
                       paste0(hour - 12, " PM"))))
}

# Load and preprocess the data
all_forecasts <- read.csv("rodriguez_schools_July152025.csv") %>%
  mutate(time = as.POSIXct(time))
all_forecasts <- all_forecasts %>%
  mutate(time = as.numeric(hour(time))) %>%
  filter(time > 3 & time < 23) %>%
  arrange(school, time)
all_forecasts$time <- format_hour_ampm(all_forecasts$time)

# UI
ui <- fluidPage(
  titlePanel("Hourly Weather Forecast per School July 15, 2025"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("school", "Select a School:",
                  choices = sort(unique(all_forecasts$school)),
                  selected = unique(all_forecasts$school)[1])
    ),
    
    mainPanel(
      h3(textOutput("school_title")),
      tableOutput("forecast_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Update title based on selected school
  output$school_title <- renderText({
    paste("Hourly Forecast for:", input$school)
  })
  
  # Render table for selected school
  output$forecast_table <- renderTable({
    all_forecasts %>%
      filter(school == input$school) %>%
      select(
        Time = time,
        Condition = condition,
        `Precip (mm)` = precip_mm,
        `Humidity (%)` = humidity,
        `Temp (°C)` = temp_c,
        `Wind (kph)` = wind_kph
      )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
}

# Run the application
shinyApp(ui = ui, server = server)
