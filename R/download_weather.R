weather_variable <- c("minimum temperature", "maximum temperature")
year <- 2022:2024
source <- "gridmet"


#' Creates the filename needed to download the weather data
#'
#' @param year A numeric vector of years
#' @param weather_variable A character vector of weather variables. The variables can be "minimum temperature", "maximum temperature", "precipitation", "relative humidity", "solar radiation", and "wind speed"
#' @param source A character vector of the source of the weather data. The source can be "gridmet"
#'
#' @returns A character vector of filenames
#'
#' @examples
#' create_weather_filename(year = 2022:2024, weather_variable = c("minimum temperature", "maximum temperature"))
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
