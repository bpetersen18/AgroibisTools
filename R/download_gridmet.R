weather_variable <- c("minimum temperature", "maximum temperature")
year <- 2022:2024
source <- "gridmet"

create_weather_filename <- function(year, weather_variable, source = "gridmet") {
  if (source == "gridmet"){
    if ("relative humidity" %in% weather_variable){
      # Remove relative humidity from weather_variable
      weather_variable <- weather_variable[weather_variable != "relative humidity"]

      # Add maximum and minimum relative humidity
      weather_variable <- c(weather_variable, "minimum relative humidity", "maximum relative humidity")
    }

    expand.grid(year = year, weather_variable = weather_variable) |>
      dplyr::mutate(weather_variable = dplyr::case_when(
        weather_variable == "minimum temperature" ~ "tmmn",
        weather_variable == "maximum temperature" ~ "tmmx",
        weather_variable == "precipitation" ~ "pr",
        weather_variable == "minimum relative humidity" ~ "rmin",
        weather_variable == "maximum relative humidity" ~ "rmax",
        weather_variable == "solar radiation" ~ "srad",
        weather_variable == "wind speed" ~ "vs",
      )) |>
      dplyr::mutate(filename = paste0(weather_variable, "_", year, ".nc")) |>
      dplyr::pull(filename)
  } else {
    stop("source not supported")

  }
}
