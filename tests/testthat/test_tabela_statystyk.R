test_that("Tabela_statystyk przyjmuje 4 argumenty i zwraca obiekt ramke danych",{
  jeden <- c(1,2,3,4,5,6,7,8,9,9,8,7,6,5,4,3,2,1)
  dwa <- c(1,2,3,4,5,6,7,8,9,9,8,7,6,5,4,3,2,1)
  trzy <- c(1,2,3,4,5,6,7,8,9,9,8,7,6,5,4,3,2,1)
  jeden <- as.data.frame(jeden)
  dwa <- as.data.frame(dwa)
  trzy <- as.data.frame(trzy)
  colnames(jeden) <- "Liczba_ruchow"
  colnames(dwa) <- "Liczba_ruchow"
  colnames(trzy) <- "Liczba_ruchow"
  expect_is(tabela_statystyk(jeden,dwa,trzy,"CosTam"),"data.frame")
  expect_error(tabela_statystyk(jeden,dwa,"CosTam"))
})