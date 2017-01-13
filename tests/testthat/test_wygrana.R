test_that("Wygrywanie przyjmuje dwa wektory i zwraca booleana",{
  expect_error(.wygrana())
  expect_error(.wygrana(c(1,1,1,1,1,0,0)))
  expect_true(is.logical(.wygrana(c(1,1,1,1,1,0,0),c(1,24,0,0,0,4,0))))
  expect_length(.wygrana(c(1,1,1,1,1,0,0),c(1,24,0,0,0,4,0)),1)
})

test_that("Wygrywanie sprawdza warunki poprawnie",{
  expect_true(.wygrana(c(1,1,1,1,1,0,0),c(1,1,1,1,1,0,0)))
  expect_false(.wygrana(c(1,1,1,1,1,0,0),c(1,1,1,0,3,0,0)))
  expect_true(.wygrana(c(1,1,1,1,1,0,0),c(4,3,3,2,4,2,3)))
  })
