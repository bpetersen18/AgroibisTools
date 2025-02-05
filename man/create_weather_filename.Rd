% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_gridmet.R
\name{create_weather_filename}
\alias{create_weather_filename}
\title{Creates the filename needed to download the weather data}
\usage{
create_weather_filename(year, weather_variable, source = "gridmet")
}
\arguments{
\item{year}{A numeric vector of years}

\item{weather_variable}{A character vector of weather variables. The variables can be "minimum temperature", "maximum temperature", "precipitation", "relative humidity", "solar radiation", and "wind speed"}

\item{source}{A character vector of the source of the weather data. The source can be "gridmet"}
}
\value{
A character vector of filenames
}
\description{
Creates the filename needed to download the weather data
}
\examples{
create_weather_filename(year = 2022:2024, weather_variable = c("minimum temperature", "maximum temperature"))
}
