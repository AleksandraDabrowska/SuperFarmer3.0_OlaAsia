test_that("Stado max to nazwany wektor o dlugosci 7",{
  stado_max <- c(60,24,20,12,6,4,2)
  names(stado_max) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')
  expect_true(is.vector(stado_max))
  expect_is(stado_max,"numeric")
  expect_length(stado_max,7)
  expect_named(stado_max)
})

test_that("Nazwy wektora sa prawidlowe i odpowiadaja dobrym liczbom krolikow",{
  stado_max <- c(60,24,20,12,6,4,2)
  names(stado_max) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')
  expect_named(stado_max,c('krolik', 'owca','swinia','krowa','kon','maly_pies','duzy_pies'))
  expect_true(all(stado_max == c(60,24,20,12,6,4,2)))
})
