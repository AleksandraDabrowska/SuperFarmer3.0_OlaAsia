test_that("Wizytowka_SuperFarmer3.0_OlaAsia przyjmuje argument strategii oraz opcjonalnie liczbe powtorzen",{
  expect_error(wizytowka_SuperFarmer3.0_OlaAsia())
  expect_error(wizytowka_SuperFarmer3.0_OlaAsia(1000))
  })