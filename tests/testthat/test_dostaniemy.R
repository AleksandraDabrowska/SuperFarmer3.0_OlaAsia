test_that("Dostaniemy przyjmuje 3 argumenty i zwraca wektor numeryczny o dlugosci 7",{
  expect_error(.dostaniemy())
  expect_error(.dostaniemy(c(1,0,0,0,0,0,0)))
  expect_error(.dostaniemy(c(1,0,0,0,0,0,0),c(0,0,0,0,0,0,0)))
  expect_true(is.vector(.dostaniemy(c(1,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2))))
  expect_is(.dostaniemy(c(1,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2)),"numeric")
  expect_named(.dostaniemy(c(1,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2)),NULL)
  expect_length(.dostaniemy(c(1,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2)),7)
})

test_that("Dostaniemy zwraca prawidlowe wyniki",{
  expect_equal(.dostaniemy(c(1,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2)),c(1,0,0,0,0,0,0))
  expect_equal(.dostaniemy(c(1,25,0,0,0,5,0),c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2)),c(1,24,0,0,0,4,0))
})
