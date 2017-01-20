test_that("Funkcja zapisz przyjmuje dwa argumenty",{
  expect_error(zapisz(plik = "Lalala"))
  expect_error(zapisz(dane))
  
})
