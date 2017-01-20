test_that("Wykres_wszytskie_zwierzatka przyjmuje argument w postaci ramki danych i zwraca wykres",{
  dana <- c(1,2,3,4,5,6,7)
  expect_error(wykres_wszytskie_zwierzatka(dana,"Zwierzaczki"))
  dana <- cbind(dana, dana)
  dana <- as.data.frame(dana)
  dana <- cbind(dana, sample("pancernik",7, replace=TRUE))
  colnames(dana) <- c("Numer_kolejki","liczba","zwierze")
  expect_is(wykres_zwierzat(dana,"Pancernik"),"ggplot")
})