test_that("Dokonanie wymian przyjmuje dwa wektory i zwraca wektor",{
  expect_error(.dokonanie_wymian())
  expect_error(.dokonanie_wymian(c(1,1,1,1,1,0,0)))
  expect_is(.dokonanie_wymian(c(0,1,1,0,0,1,0),c(6,-1,0,0,0,0,0)),"numeric")
  expect_named(.dokonanie_wymian(c(0,1,1,0,0,1,0),c(6,-1,0,0,0,0,0)),NULL)
  expect_length(.dokonanie_wymian(c(0,1,1,0,0,1,0),c(6,-1,0,0,0,0,0)),7)
})

test_that("Dokonanie wymian dodaje wymiane poprawnie",{
  expect_equal(.dokonanie_wymian(c(0,1,1,0,0,1,0),c(6,-1,0,0,0,0,0)),c(6,0,1,0,0,1,0))
})
