test_that("Wykres_gestosci przyjmuje argument w postaci ramki danych",{
          dana <- c(1,2,3,4,5,6,7)
          mediana <- median(dana)
          srednia <- mean(dana)
          expect_error(wykres_gestosci(dana,"Lala",mediana, srednia))})
