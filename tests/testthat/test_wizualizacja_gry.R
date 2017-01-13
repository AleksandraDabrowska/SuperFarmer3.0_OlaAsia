test_that("Wizualizacja gry przyjmuje wektor czasow trwania poszczegolnych gier oraz typy wykresow",{
          wektor <- c(20,30,30,20,30,20,10,12)
          expect_error(wizualizacja_gry())
          expect_length(wizualizacja_gry(wektor),9)
          expect_length(wizualizacja_gry(badaj_gre(strategia_owce,powtorzenia=10)),9)
          expect_length(wizualizacja_gry(wektor,typ_wykresu1="histogram"),9)
          expect_length(wizualizacja_gry(wektor,typ_wykresu1="pudelkowy"),9)
          expect_length(wizualizacja_gry(wektor,typ_wykresu1="skrzypcowy"),9)
          expect_length(wizualizacja_gry(wektor,typ_wykresu1="skrzypcowy",typ_wykresu2="pudelkowy"),9)
          })

test_that("Wizualizacja gry zwraca poprawne wartosci",{
  wektor <- c(20,30,30,20,30,20,10,12)
  expect_is(wizualizacja_gry(wektor,typ_wykresu1 ="histogram"),c("gg","ggplot"))
  expect_is(wizualizacja_gry(badaj_gre(strategia_owce,powtorzenia = 10),typ_wykresu1 ="skrzypcowy"),c("gg","ggplot"))
})
