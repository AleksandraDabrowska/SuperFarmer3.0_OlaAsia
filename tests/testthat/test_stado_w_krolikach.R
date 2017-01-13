test_that("Funkcja stado_w_krolikach przyjmuje dwa argumenty i zwraca liczbe",{
  stado <- c(1,1,1,1,1,0,0)
  names(stado) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
  wartosc_w_krolikach <- c(1,6,12,36,72,6,36)
  names(wartosc_w_krolikach) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
  expect_error(.stado_w_krolikach())
  expect_error(.stado_w_krolikach(stado))
  expect_error(.stado_w_krolikach(wartosc_w_krolikach))
  expect_true(is.numeric(.stado_w_krolikach(wartosc_w_krolikach,stado)))
})

test_that("Funkcja stado_w_krolikach zwraca poprawne wartosci",{
  stado <- c(1,1,1,1,1,0,0)
  names(stado) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
  wartosc_w_krolikach <- c(1,6,12,36,72,6,36)
  names(wartosc_w_krolikach) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
  expect_equivalent(.stado_w_krolikach(wartosc_w_krolikach,stado),127)
})
