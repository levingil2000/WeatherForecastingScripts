library(lubridate)
##TomorrowIO weather forecast
# Replace with your actual Tomorrow.io API key
api_key <- "PyTpxNDg8Syy5KnmSblADQKeoIH7OwZl"

# Tomorrow.io forecast API endpoint
base_url <- "https://api.tomorrow.io/v4/timelines"

# Get current date and time in your local timezone (Las Pinas City)
# It's important to be mindful of timezones. Tomorrow.io often works with UTC.
# Let's set it to tomorrow's start and end in UTC for consistency.
current_date_local <- Sys.Date() # Gets today's date in local timezone

# Calculate tomorrow's date
tomorrow_date <- current_date_local + days(1)

# Define start and end times for tomorrow in UTC
start_time_utc <- paste0(tomorrow_date, "T00:00:00Z")
end_time_utc <- paste0(tomorrow_date, "T23:59:59Z")

# Define the fields you want to retrieve
fields <- c(
  "temperature", "precipitationIntensity",
  "precipitationType", "windSpeed", "windDirection",
  "humidity", "cloudCover", "weatherCode"
)

# Convert fields to a comma-separated string for the API request
fields_string <- paste(fields, collapse = ",")

# Initialize an empty list to store results
all_forecast_data <- list()

# Loop through each location in your geocoded dataset
for (i in 1:nrow(geocoded)) {
  lat <- geocoded$latitude[i]
  lon <- geocoded$longitude[i]
  location_name <- geocoded$location_name[i] # Assuming you have a name column
  
  # Construct the API request URL
  # We use 'timesteps=1h' for hourly forecast
  # We specify 'units=metric' for Celsius, m/s etc. (change to 'imperial' if preferred)
  request_url <- paste0(
    base_url,
    "?location=", lat, ",", lon,
    "&fields=", fields_string,
    "&units=metric", # Or 'imperial' for Fahrenheit, mph etc.
    "&timesteps=1h",
    "&startTime=", start_time_utc,
    "&endTime=", end_time_utc,
    "&apikey=", api_key
  )
  
  # Make the API request
  response <- GET(request_url)
  
  # Check if the request was successful (status code 200)
  if (http_status(response)$category == "Success") {
    # Parse the JSON response
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    
    # Extract hourly data
    # The hourly data is nested within timelines$hourly$intervals
    hourly_intervals <- data$data$timelines$intervals[[1]]$hourly
    
    # Check if there's hourly data available
    if (!is.null(hourly_intervals) && length(hourly_intervals) > 0) {
      # Convert to a data frame
      hourly_df <- as.data.frame(hourly_intervals)
      
      # Add location information
      hourly_df$latitude <- lat
      hourly_df$longitude <- lon
      hourly_df$location_name <- location_name
      
      # Store the data frame in our list
      all_forecast_data[[i]] <- hourly_df
    } else {
      warning(paste("No hourly forecast data found for location:", location_name, "(", lat, ",", lon, ")"))
    }
    
  } else {
    warning(paste("API request failed for location:", location_name, "(", lat, ",", lon, ") - Status:", http_status(response)$reason))
    print(content(response, "text", encoding = "UTF-8")) # Print error message from API
  }
  
  # Be mindful of API rate limits. Add a small delay if making many requests.
  Sys.sleep(0.1) # Pause for 0.1 seconds
}

# Combine all data frames into one
final_forecast_df <- bind_rows(all_forecast_data)

# View the first few rows of the combined data
head(final_forecast_df)

# You can now save this data to a CSV or use it for further analysis
# write.csv(final_forecast_df, "tomorrow_hourly_forecast.csv", row.names = FALSE)