test_that("Zamiana wektora napisowego rzutu daje wektor liczbowy",{
  kostka_czerwona <- c("krolik","krolik","krolik","krolik", "krolik","krolik","owca","owca","swinia","swinia","kon","lis")
  kostka_zielona <- c("krolik","krolik","krolik","krolik", "krolik","krolik","owca","owca","owca","swinia","krowa","wilk")
  expect_error(.zamien_wynik_rzutu_na_wektor())
  expect_true(is.vector(.zamien_wynik_rzutu_na_wektor(.rzut_kostkami(kostka_zielona,kostka_czerwona))))
  expect_true(is.vector(.zamien_wynik_rzutu_na_wektor(c('krolik','wilk'))))
  expect_is(.zamien_wynik_rzutu_na_wektor(.rzut_kostkami(kostka_zielona,kostka_czerwona)),"numeric")
  expect_named(.zamien_wynik_rzutu_na_wektor(.rzut_kostkami(kostka_zielona,kostka_czerwona)),NULL)
  expect_length(.zamien_wynik_rzutu_na_wektor(c('krolik','wilk')),7)
})

test_that("Wyniki zamiany sa poprawne",{
  expect_equal(.zamien_wynik_rzutu_na_wektor(c('krolik','wilk')),c(1,0,0,0,0,0,0))
  expect_equal(.zamien_wynik_rzutu_na_wektor(c('owca','owca')),c(0,2,0,0,0,0,0))
  expect_equal(.zamien_wynik_rzutu_na_wektor(c('swinia','kon')),c(0,0,1,0,1,0,0))
})
