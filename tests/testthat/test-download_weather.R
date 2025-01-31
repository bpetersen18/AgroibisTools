test_that("create_weather_filename() creates the correct filename", {
  expect_equal(create_weather_filename(year = 2022:2024, weather_variable = c("minimum temperature", "maximum temperature", "precipitation", "relative humidity")), c("tmmn_2022.nc", "tmmn_2023.nc", "tmmn_2024.nc", "tmmx_2022.nc", "tmmx_2023.nc", "tmmx_2024.nc", "pr_2022.nc", "pr_2023.nc", "pr_2024.nc", "rmin_2022.nc", "rmin_2023.nc", "rmin_2024.nc", "rmax_2022.nc", "rmax_2023.nc", "rmax_2024.nc"))
})
