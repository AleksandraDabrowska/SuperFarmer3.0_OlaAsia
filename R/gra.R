#'Funkcja opisujaca przebieg gry
#'
#'Funkcja `gra()` symuluje pojedyncza rozgrywke w SuperFarmera zadana na poczatku strategia. Opcjonalnie mozna zmieniac parametry, np. zwierzeta na kostkach, warunek wygranej. Funkcja zwraca liczbe oznaczajaca dlugosc trwania tejze gry.
#'
#'@param strategia nazwa funkcji opisujacej strategie gracza
#'@param stado_max maksymalny stan stada glownego
#'@param kostka_zielona zwierzeta na kostce zielonej
#'@param kostka_czerwona zwierzeta na kostce czerwonej
#'@param warunek_wygranej warunki konieczne do zakonczenia gry
#'@param co_zostawia_lis wektor zawierajacy liczbe zwierzat ktora maksymalnie zostaje w stadzie po ataku lisa
#'@param co_zostawia_wilk wektor zawierajacy liczbe zwierzat ktora maksymalnie zostaje w stadzie po ataku wilka
#'@param wartosc_w_krolikach wektor zawierajacy wartosci w krolikach poszczegolnych zwierzat
#'
#'@return Lista dwuelementowa - 1. macierz zawierajaca liczbę zwierzat w stadzie po kazdym ruchu;
#'2. liczbe kolejek, ktora trwala gra.
#'
#'@export

gra <- function(strategia,stado_max=SuperFarmer.SuperDziewczyn::stado_max,kostka_zielona=SuperFarmer.SuperDziewczyn::kostka_zielona,kostka_czerwona=SuperFarmer.SuperDziewczyn::kostka_czerwona,warunek_wygranej=SuperFarmer.SuperDziewczyn::warunek_wygranej,co_zostawia_lis=SuperFarmer.SuperDziewczyn::co_zostawia_lis,co_zostawia_wilk=SuperFarmer.SuperDziewczyn::co_zostawia_wilk,wartosc_w_krolikach=SuperFarmer.SuperDziewczyn::wartosc_w_krolikach){
  stado <- numeric(7)
  names(stado) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')
  if (is.null(names(wartosc_w_krolikach))){
    names(wartosc_w_krolikach) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')
  }
  licznik <- 0
  Macierz <- c()
  if(!is.null(strategia)){
  while(.wygrana(warunek_wygranej,stado)!=TRUE){
    stado <- tryCatch(strategia(stado,wartosc_w_krolikach=wartosc_w_krolikach),error = function(e) strategia(stado), finally= {})
    names(stado) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')
    stado <- .przebieg_rzutu(stado,stado_max,kostka_zielona,kostka_czerwona,co_zostawia_lis,co_zostawia_wilk)
    Macierz<-rbind(Macierz,stado)
    licznik <- licznik+1
  }}
  rownames(Macierz) <- paste(1:licznik)
  return(list(Macierz,licznik))
}
