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
  if (!(source %in% c("gridmet"))){
    stop("source not supported")
  }

  if (!is.numeric(year)){
    stop("is.numeric(year) is not TRUE")
  }

  if (!is.character(weather_variable)){
    stop("is.character(weather_variable) is not TRUE")
  }

  if (!is.character(source)){
    stop("is.character(source) is not TRUE")
  }

  if (!(length(year) > 0)){
    stop("length(year) > 0 is not TRUE")
  }

  if (!(length(weather_variable) > 0)){
    stop("length(weather_variable) > 0 is not TRUE")
  }

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

  }
}


#' Downloads and converts weather data to be used in agro_ibis
#'
#' @param year A numeric vector of years
#' @param weather_variable A character vector of weather variables. The variables can be "minimum temperature", "maximum temperature", "precipitation", "relative humidity", "solar radiation", and "wind speed"
#' @param source A character vector of the source of the weather data. The source can be "gridmet"
#' @param outdir A character vector of the directory where the weather data will be stored
#'
#' @returns
#' @export
#'
#' @examples
get_weather <- function(year, weather_variable, source = "gridmet", outdir) {

    filename <- create_weather_filename(year = year, weather_variable = weather_variable, source = source)

  if (source == "gridmet"){
    for (i in 1:length(filename)){
      url <- paste0("https://www.northwestknowledge.net/metdata/data/", filename[i])
      download.file(url, destfile = paste0(outdir, "/", filename[i]), mode = "wb")

      # Check if the file was downloaded
      if (!file.exists(paste0(outdir, "/", filename[i]))){
        stop("File not downloaded")
      }

    }

    # Create agro_ibis directory inside outdir where the weather data will be stored
    dir.create(file.path(outdir, "agro_ibis"), showWarnings = FALSE)

    for (variable in weather_variable){
      if (variable == "precipitation"){
        # List all precipitation files
        precip_files <- list.files(path = outdir, pattern = "pr", full.names = TRUE)

        # Loop through precipitation files
        for (precip_file in precip_files){
          # Remove the "crs" variable from the netcdf file using ncdf4
          nc <- ncdf4::nc_open(precip_file, write = FALSE)

          # Get dimension information
          day_dim <- nc$dim$day
          lat_dim <- nc$dim$lat
          lon_dim <- nc$dim$lon

          # Rename "day" to "time"
          time_dim <- ncdf4::ncdim_def(name = "time", units = "days since 1900-01-01 00:00:00", vals = day_dim$vals, longname = "Time")

          # Define a new "lev" dimension (size = 1)
          lev_dim <- ncdf4::ncdim_def(name = "lev", units = "NA", vals = 1, longname = "2 meters above ground")

          # Get "precipitation_amount" variable details
          precip_var <- nc$var$precipitation_amount
          precip_data <- ncdf4::ncvar_get(nc, "precipitation_amount")

          # Reshape precipitation data to add "lev" dimension from [1386, 585, 365] to [365, 1, 585, 1386] without changing the spatial structure
          new_precip_data <- aperm(precip_data, c(3, 2, 1))
          new_precip_data <- array(new_precip_data, dim = c(day_dim$len, 1, lat_dim$len, lon_dim$len))

          # Define the new precipitation_amount variable with reordered dimensions
          new_precip_var <- ncdf4::ncvar_def(name = "precipitation_amount", units = precip_var$units, dim = list(time_dim, lev_dim, lat_dim, lon_dim), missval = precip_var$missval, longname = precip_var$longname)

          # Create a new NetCDF file with modified dimensions
          new_nc <- ncdf4::nc_create(paste0(outdir, "/agro_ibis/", basename(precip_file)), list(new_precip_var))

          # Write the modified precipitation data to the new NetCDF file
          ncdf4::ncvar_put(new_nc, new_precip_var, new_precip_data)

          # Close files
          ncdf4::nc_close(nc)
          ncdf4::nc_close(new_nc)

          # Clean environment variables defined in the loop
          rm(nc, day_dim, lat_dim, lon_dim, time_dim, lev_dim, precip_var, precip_data, new_precip_data, new_precip_var, new_nc)
        }
    } else if (variable == "relative humidity"){
      # List all max relative humidity files
      rmax_files <- list.files(path = outdir, pattern = "rmax", full.names = TRUE)

      # List all min relative humidity files
      rmin_files <- list.files(path = outdir, pattern = "rmin", full.names = TRUE)

      # Loop through the relative humidity files
      for (i in 1:length(rmax_files)){
        # Open the NetCDF files
        nc_rmax <- ncdf4::nc_open(rmax_files[i], write = FALSE)
        nc_rmin <- ncdf4::nc_open(rmin_files[i], write = FALSE)

        # Get dimension information
        day_dim <- nc_rmax$dim$day
        lat_dim <- nc_rmax$dim$lat
        lon_dim <- nc_rmax$dim$lon

        # Rename "day" to "time"
        time_dim <- ncdf4::ncdim_def(name = "time", units = "days since 1900-01-01 00:00:00", vals = day_dim$vals, longname = "Time")

        # Define a new "lev" dimension (size = 1)
        lev_dim <- ncdf4::ncdim_def(name = "lev", units = "NA", vals = 1, longname = "2 meters above ground")

        # Get "relative_humidity" variable details
        rmax_var <- nc_rmax$var$relative_humidity
        rmin_var <- nc_rmin$var$relative_humidity
        rmax_data <- ncdf4::ncvar_get(nc_rmax, "relative_humidity")
        rmin_data <- ncdf4::ncvar_get(nc_rmin, "relative_humidity")

        # Reshape relative humidity data to add "lev" dimension from [1386, 585, 365] to [365, 1, 585, 1386] without changing the spatial structure
        new_rmax_data <- aperm(rmax_data, c(3, 2, 1))
        new_rmax_data <- array(new_rmax_data, dim = c(day_dim$len, 1, lat_dim$len, lon_dim$len))

        new_rmin_data <- aperm(rmin_data, c(3, 2, 1))
        new_rmin_data <- array(new_rmin_data, dim = c(day_dim$len, 1, lat_dim$len, lon_dim$len))

        # Calculate the average relative humidity
        new_rh_data <- (new_rmax_data + new_rmin_data) / 2

        # Define the new relative_humidity variable with reordered dimensions
        new_rh_var <- ncdf4::ncvar_def(name = "relative_humidity", units = rmax_var$units, dim = list(time_dim, lev_dim, lat_dim, lon_dim), missval = rmax_var$missval, longname = "relative humidity")
        new_rmax_var <- ncdf4::ncvar_def(name = "rmax", units = rmax_var$units, dim = list(time_dim, lev_dim, lat_dim, lon_dim), missval = rmax_var$missval, longname = "max relative humidity")
        new_rmin_var <- ncdf4::ncvar_def(name = "rmin", units = rmax_var$units, dim = list(time_dim, lev_dim, lat_dim, lon_dim), missval = rmax_var$missval, longname = "min relative humidity")

        # Create a new NetCDF file with modified dimensions
        new_rh_nc <- ncdf4::nc_create(paste0(outdir, "/agro_ibis/", gsub("rmax_", "rh_", basename(rmax_files[i]))), list(new_rh_var))
        new_rmax_nc <- ncdf4::nc_create(paste0(outdir, "/agro_ibis/", basename(rmax_files[i])), list(new_rmax_var))
        new_rmin_nc <- ncdf4::nc_create(paste0(outdir, "/agro_ibis/", basename(rmin_files[i])), list(new_rmin_var))

        # Write the modified relative humidity data to the new NetCDF file
        ncdf4::ncvar_put(new_rh_nc, new_rh_var, new_rh_data)
        ncdf4::ncvar_put(new_rmax_nc, new_rmax_var, new_rmax_data)
        ncdf4::ncvar_put(new_rmin_nc, new_rmin_var, new_rmin_data)

        # Close files
        ncdf4::nc_close(nc_rmax)
        ncdf4::nc_close(nc_rmin)
        ncdf4::nc_close(new_rh_nc)
        ncdf4::nc_close(new_rmax_nc)
        ncdf4::nc_close(new_rmin_nc)

        # Clean environment variables defined in the loop
        rm(nc_rmax, nc_rmin, rmax_var, rmin_var, rmax_data, rmin_data, new_rmax_data, new_rmin_data, new_rh_data, new_rh_var, new_rmax_var, new_rmin_var, new_rh_nc, new_rmax_nc, new_rmin_nc)
      }
    } else if (variable == "solar radiation") {
      # List all solar radiation files
      srad_files <- list.files(path = outdir, pattern = "srad", full.names = TRUE)

      # Loop through solar radiation files
      for (srad_file in srad_files){
        # Remove the "crs" variable from the netcdf file using ncdf4
        nc <- ncdf4::nc_open(srad_file, write = FALSE)

        # Get dimension information
        day_dim <- nc$dim$day
        lat_dim <- nc$dim$lat
        lon_dim <- nc$dim$lon

        # Rename "day" to "time"
        time_dim <- ncdf4::ncdim_def(name = "time", units = "days since 1900-01-01 00:00:00", vals = day_dim$vals, longname = "Time")

        # Define a new "lev" dimension (size = 1)
        lev_dim <- ncdf4::ncdim_def(name = "lev", units = "NA", vals = 1, longname = "2 meters above ground")

        # Get "surface_downwelling_shortwave_flux_in_air" variable details
        srad_var <- nc$var$surface_downwelling_shortwave_flux_in_air
        srad_data <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")

        # Reshape solar radiation data to add "lev" dimension from [1386, 585, 365] to [365, 1, 585, 1386] without changing the spatial structure
        new_srad_data <- aperm(srad_data, c(3, 2, 1))
        new_srad_data <- array(new_srad_data, dim = c(day_dim$len, 1, lat_dim$len, lon_dim$len))

        # Define the new solar radiation variable with reordered dimensions
        new_srad_var <- ncdf4::ncvar_def(name = "surface_downwelling_shortwave_flux_in_air", units = srad_var$units, dim = list(time_dim, lev_dim, lat_dim, lon_dim), missval = srad_var$missval, longname = srad_var$longname)

        # Create a new NetCDF file with modified dimensions
        new_nc <- ncdf4::nc_create(paste0(outdir, "/agro_ibis/", basename(srad_file)), list(new_srad_var))

        # Write the modified precipitation data to the new NetCDF file
        ncdf4::ncvar_put(new_nc, new_srad_var, new_srad_data)

        # Close files
        ncdf4::nc_close(nc)
        ncdf4::nc_close(new_nc)

        # Clean environment variables defined in loop
        rm(srad_file, nc, day_dim, lat_dim, lon_dim, time_dim, lev_dim, srad_var, srad_data, new_srad_data, new_srad_var, new_nc)

      }
    } else if (variable == "minimum temperature") {
      # List all minimum temperature files
      tmmn_files <- list.files(path = outdir, pattern = "tmmn", full.names = TRUE)

      # Loop through minimum temperature files
      for (tmmn_file in tmmn_files){
        # Remove the "crs" variable from the netcdf file using ncdf4
        nc <- ncdf4::nc_open(tmmn_file, write = FALSE)

        # Get dimension information
        day_dim <- nc$dim$day
        lat_dim <- nc$dim$lat
        lon_dim <- nc$dim$lon

        # Rename "day" to "time"
        time_dim <- ncdf4::ncdim_def(name = "time", units = "days since 1900-01-01 00:00:00", vals = day_dim$vals, longname = "Time")

        # Define a new "lev" dimension (size = 1)
        lev_dim <- ncdf4::ncdim_def(name = "lev", units = "NA", vals = 1, longname = "2 meters above ground")

        # Get "air_temperature" variable details
        tmmn_var <- nc$var$air_temperature
        tmmn_data <- ncdf4::ncvar_get(nc, "air_temperature")

        # Convert from Kelvin to Celsius
        tmmn_data <- tmmn_data - 273.15

        # Reshape minimum temperature data to add "lev" dimension from [1386, 585, 365] to [365, 1, 585, 1386] without changing the spatial structure
        new_tmmn_data <- aperm(tmmn_data, c(3, 2, 1))
        new_tmmn_data <- array(new_tmmn_data, dim = c(day_dim$len, 1, lat_dim$len, lon_dim$len))

        # Define the new minimum temperature variable with reordered dimensions
        new_tmmn_var <- ncdf4::ncvar_def(name = "air_temperature", units = "degC", dim = list(time_dim, lev_dim, lat_dim, lon_dim), missval = tmmn_var$missval, longname = tmmn_var$longname)

        # Create a new NetCDF file with modified dimensions
        new_nc <- ncdf4::nc_create(paste0(outdir, "/agro_ibis/", basename(tmmn_file)), list(new_tmmn_var))

        # Write the modified precipitation data to the new NetCDF file
        ncdf4::ncvar_put(new_nc, new_tmmn_var, new_tmmn_data)

        # Close files
        ncdf4::nc_close(nc)
        ncdf4::nc_close(new_nc)

        # Clean environment variables defined in the loop
        rm(tmmn_file, nc, day_dim, lat_dim, lon_dim, time_dim, lev_dim, tmmn_var, tmmn_data, new_tmmn_data, new_tmmn_var, new_nc)
        }
    } else if (variable == "maximum temperature") {
      # List all maximum temperature files
      tmmx_files <- list.files(path = outdir, pattern = "tmmx", full.names = TRUE)

      # Loop through maximum temperature files
      for (tmmx_file in tmmx_files){
        # Remove the "crs" variable from the netcdf file using ncdf4
        nc <- ncdf4::nc_open(tmmx_file, write = FALSE)

        # Get dimension information
        day_dim <- nc$dim$day
        lat_dim <- nc$dim$lat
        lon_dim <- nc$dim$lon

        # Rename "day" to "time"
        time_dim <- ncdf4::ncdim_def(name = "time", units = "days since 1900-01-01 00:00:00", vals = day_dim$vals, longname = "Time")

        # Define a new "lev" dimension (size = 1)
        lev_dim <- ncdf4::ncdim_def(name = "lev", units = "NA", vals = 1, longname = "2 meters above ground")

        # Get "air_temperature" variable details
        tmmx_var <- nc$var$air_temperature
        tmmx_data <- ncdf4::ncvar_get(nc, "air_temperature")

        # Convert from Kelvin to Celsius
        tmmx_data <- tmmx_data - 273.15

        # Reshape maximum temperature data to add "lev" dimension from [1386, 585, 365] to [365, 1, 585, 1386] without changing the spatial structure
        new_tmmx_data <- aperm(tmmx_data, c(3, 2, 1))
        new_tmmx_data <- array(new_tmmx_data, dim = c(day_dim$len, 1, lat_dim$len, lon_dim$len))

        # Define the new maximum temperature variable with reordered dimensions
        new_tmmx_var <- ncdf4::ncvar_def(name = "air_temperature", units =  "degC", dim = list(time_dim, lev_dim, lat_dim, lon_dim), missval = tmmx_var$missval, longname = tmmx_var$longname)

        # Create a new NetCDF file with modified dimensions
        new_nc <- ncdf4::nc_create(paste0(outdir, "/agro_ibis/", basename(tmmx_file)), list(new_tmmx_var))

        # Write the modified precipitation data to the new NetCDF file
        ncdf4::ncvar_put(new_nc, new_tmmx_var, new_tmmx_data)

        # Close files
        ncdf4::nc_close(nc)
        ncdf4::nc_close(new_nc)

        # Clean environment variables defined in the loop
        rm(tmmx_file, nc, day_dim, lat_dim, lon_dim, time_dim, lev_dim, tmmx_var, tmmx_data, new_tmmx_data, new_tmmx_var, new_nc)
      }
    } else if (variable == "wind speed") {
        # List all wind speed files
        vs_files <- list.files(path = outdir, pattern = "vs", full.names = TRUE)

        # Loop through maximum temperature files
        for (vs_file in vs_files){
          # Remove the "crs" variable from the netcdf file using ncdf4
          nc <- ncdf4::nc_open(vs_file, write = FALSE)

          # Get dimension information
          day_dim <- nc$dim$day
          lat_dim <- nc$dim$lat
          lon_dim <- nc$dim$lon

          # Rename "day" to "time"
          time_dim <- ncdf4::ncdim_def(name = "time", units = "days since 1900-01-01 00:00:00", vals = day_dim$vals, longname = "Time")

          # Define a new "lev" dimension (size = 1)
          lev_dim <- ncdf4::ncdim_def(name = "lev", units = "NA", vals = 1, longname = "2 meters above ground")

          # Get "wind_speed" variable details
          vs_var <- nc$var$wind_speed
          vs_data <- ncdf4::ncvar_get(nc, "wind_speed")

          # Reshape maximum temperature data to add "lev" dimension from [1386, 585, 365] to [365, 1, 585, 1386] without changing the spatial structure
          new_vs_data <- aperm(vs_data, c(3, 2, 1))
          new_vs_data <- array(new_vs_data, dim = c(day_dim$len, 1, lat_dim$len, lon_dim$len))

          # Define the new maximum temperature variable with reordered dimensions
          new_vs_var <- ncdf4::ncvar_def(name = "wind_speed", units = vs_var$units, dim = list(time_dim, lev_dim, lat_dim, lon_dim), missval = vs_var$missval, longname = vs_var$longname)

          # Create a new NetCDF file with modified dimensions
          new_nc <- ncdf4::nc_create(paste0(outdir, "/agro_ibis/", basename(vs_file)), list(new_vs_var))

          # Write the modified precipitation data to the new NetCDF file
          ncdf4::ncvar_put(new_nc, new_vs_var, new_vs_data)

          # Close files
          ncdf4::nc_close(nc)
          ncdf4::nc_close(new_nc)

          # Clean environment variables defined in the loop
          rm(vs_file, nc, day_dim, lat_dim, lon_dim, time_dim, lev_dim, vs_var, vs_data, new_vs_data, new_vs_var, new_nc)
        }
      }
    }
  }
}
