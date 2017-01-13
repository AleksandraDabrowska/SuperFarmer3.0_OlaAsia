test_that("Dodawanie zwierzat przyjmuje przynajmniej 3 argumenty i zwraca nazwany wektor numeryczny o dlugosci 7",{
  expect_error(.dodaj_lub_odejmij_zwierzeta())
  expect_error(.dodaj_lub_odejmij_zwierzeta(c(1,24,0,0,0,4,0)))
  expect_error(.dodaj_lub_odejmij_zwierzeta(c(1,24,0,0,0,4,0),-1))
  expect_error(.dodaj_lub_odejmij_zwierzeta(c(1,24,0,0,0,4,0),-1,c(0,0,0,0,0,0,0)))
  expect_true(is.vector(.dodaj_lub_odejmij_zwierzeta(c(1,24,0,0,0,4,0),1,c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2))))
  expect_is((.dodaj_lub_odejmij_zwierzeta(c(1,24,0,0,0,4,0),1,c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2))),"numeric")
  expect_length(.dodaj_lub_odejmij_zwierzeta(c(1,24,0,0,0,4,0),1,c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2)),7)
})

test_that("Dodawanie zwierzat zwraca poprawne wartosci",{
  expect_equal(.dodaj_lub_odejmij_zwierzeta(c(0,1,0,0,0,0,0),1,c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2)),c(0,1,0,0,0,0,0))
  expect_equal(.dodaj_lub_odejmij_zwierzeta(c(2,0,0,1,0,0,0),1,c(3,1,0,0,1,0,0),c(57,23,20,12,5,4,2)),c(5,1,0,1,1,0,0))
  expect_equal(.dodaj_lub_odejmij_zwierzeta(c(1,24,0,0,0,4,0),1,c(0,0,0,0,0,0,0),c(60,24,20,12,6,4,2)),c(1,24,0,0,0,4,0))
})
