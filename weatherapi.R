# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

# Replace this with your real API key


# Filter out schools with NA coordinates
geocoded_valid <- geocoded %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Define a safe forecast extractor function
get_forecast_for_school <- function(lat, lon, school_name) {
  url <- paste0("https://api.weatherapi.com/v1/forecast.json?key=", api_key,
                "&q=", lat, ",", lon,
                "&days=1&aqi=no&alerts=no")
  
  tryCatch({
    res <- GET(url)
    
    # Ensure we received valid JSON
    if (status_code(res) != 200 ||
        !grepl("application/json", headers(res)$`content-type`)) {
      message("Failed for: ", school_name)
      return(NULL)
    }
    
    # Parse raw JSON without simplifying
    res_txt <- content(res, as = "text", encoding = "UTF-8")
    json_raw <- fromJSON(res_txt, simplifyVector = FALSE)
    
    hours <- json_raw$forecast$forecastday[[1]]$hour
    
    # Extract relevant fields using map
    hourly_data <- map_dfr(hours, function(hour) {
      tibble(
        school = school_name,
        latitude = lat,
        longitude = lon,
        time = hour$time,
        temp_c = hour$temp_c,
        condition = hour$condition$text,
        wind_kph = hour$wind_kph,
        humidity = hour$humidity,
        will_it_rain = hour$will_it_rain,
        chance_of_rain = hour$chance_of_rain,
        precip_mm = hour$precip_mm
      )
    })
    
    return(hourly_data)
  }, error = function(e) {
    message("Error for ", school_name, ": ", e$message)
    return(NULL)
  })
}

# Loop over all valid schools
all_forecasts <- pmap_dfr(
  list(
    geocoded_valid$latitude,
    geocoded_valid$longitude,
    geocoded_valid$school
  ),
  get_forecast_for_school
)

# View the first few rows
print(head(all_forecasts))

# save to CSV
write.csv(all_forecasts, "rodriguez_schools_July152025.csv", row.names = FALSE)
