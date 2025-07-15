library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# Replace with your actual API key
api_key <- api_key_tomorrowio

# Define forecast time range (UTC)
current_date_local <- Sys.Date()
tomorrow_date <- current_date_local + days(1)
start_time_utc <- paste0(tomorrow_date, "T00:00:00Z")
end_time_utc <- paste0(tomorrow_date, "T23:59:59Z")

# Weather fields
fields <- c(
  "temperature", "precipitationIntensity", "precipitationType",
  "windSpeed", "windDirection", "humidity", "cloudCover", "weatherCode"
)
fields_string <- paste(fields, collapse = ",")

# Initialize results list
results_list <- list()

# Loop through each row in your dataset
for (i in seq_len(nrow(geocoded))) {
  lat <- geocoded$latitude[i]
  lon <- geocoded$longitude[i]
  loc_id <- geocoded$school[i]  # Optional: use ID or name
  
  response <- GET(
    url = "https://api.tomorrow.io/v4/timelines",
    query = list(
      location = paste(lat, lon, sep = ","),
      fields = fields_string,
      units = "metric",
      timesteps = "1h",
      startTime = start_time_utc,
      endTime = end_time_utc,
      apikey = api_key
    )
  )
  
  if (response$status_code == 200) {
    content_text <- content(response, as = "text", encoding = "UTF-8")
    parsed <- fromJSON(content_text, flatten = TRUE)
    
    df <- parsed$data$timelines$intervals[[1]]
    names(df) <- sub("^values\\.", "", names(df))
    df$location_id <- loc_id
    df$latitude <- lat
    df$longitude <- lon
    
    results_list[[i]] <- df
  } else {
    cat("Failed for location:", loc_id, "- Status code:", response$status_code, "\n")
  }
  
  
  # Optional: delay to avoid hitting rate limits
  Sys.sleep(145)
}

# Combine all data into one data frame
weather_forecast_df <- bind_rows(results_list)
