test_that("Funkcja przygotuj dane przyjmuje jako argument wektor o dlugosci n oraz string z nazwa.",{
  wektor <- c(23,45,67,87,11,23,24,56)
  expect_is(przygotuj_dane(wektor, "Przyklad", 8),"data.frame")
  expect_equal(nrow(przygotuj_dane(wektor, "Przyklad", 8)),8)
  expect_error(przygotuj_dane("Przyklad"))
})
