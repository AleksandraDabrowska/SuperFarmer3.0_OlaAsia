test_that("Statystyki gry przyjmuje wektor czasow trwania poszczegolnych gier",{
  wektor <- c(20,30,30,20,30,20,10,12)
  expect_error(statystyki_gry())
  expect_length(statystyki_gry(wektor),6)
})


