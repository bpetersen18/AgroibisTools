test_that("create_weather_filename() creates the correct filename", {
  expect_equal(create_weather_filename(year = 2022:2024, weather_variable = c("minimum temperature", "maximum temperature", "precipitation", "relative humidity")), c("tmmn_2022.nc", "tmmn_2023.nc", "tmmn_2024.nc", "tmmx_2022.nc", "tmmx_2023.nc", "tmmx_2024.nc", "pr_2022.nc", "pr_2023.nc", "pr_2024.nc", "rmin_2022.nc", "rmin_2023.nc", "rmin_2024.nc", "rmax_2022.nc", "rmax_2023.nc", "rmax_2024.nc"))
})

#test_that("create_weather_filename() throws an error if source is not supported", {
#  expect_error(create_weather_filename(year = 2022:2024, weather_variable = "minimum temperature", source = "not_supported"), "source not supported")
#})

#test_that("create_weather_filename() throws an error if year is not numeric", {
#  expect_error(create_weather_filename(year = c("2022", "2023"), weather_variable = "minimum temperature"), "is.numeric(year) is not TRUE", regexp = TRUE)
#})

# test_that("create_weather_filename() throws an error if weather_variable is not character", {
#   expect_error(create_weather_filename(year = 2022:2024, weather_variable = c(1, 2, 3, 4)), "is.character(weather_variable) is not TRUE")
# })
#
# test_that("create_weather_filename() throws an error if year is empty", {
#   expect_error(create_weather_filename(year = numeric(0), weather_variable = c("minimum temperature", "maximum temperature", "precipitation", "relative humidity")), "length(year) > 0 is not TRUE")
# })
