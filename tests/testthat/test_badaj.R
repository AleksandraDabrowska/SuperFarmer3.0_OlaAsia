test_that("Badaj gre przyjmuje nazwe funkcji strategii",{
  expect_error(badaj_gre())
  expect_error(badaj_gre(powtorzenia = 10))
  expect_length(badaj_gre(strategia_wymian_0_0_0_0, powtorzenia = 10),10)
  expect_length(badaj_gre(strategia_wymian_1_1_1_1, powtorzenia = 10),10)
  expect_length(badaj_gre(strategia_owce, powtorzenia = 10),10)
})
