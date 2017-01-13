#'Funkcja powtarzajaca gre podana liczbe razy
#'
#'Funkcja badaj_gre() symuluje n rozgrywek w SuperFarmera zadana na poczatku strategia. Opcjonalnie mozna zmieniac parametry, np. powtorzenia, zwierzeta na kostkach, warunek wygranej. Funkcja zwraca wektor dlugosci n zawierajacy informacje o dlugosci poszczegolnych gier.
#'
#'@param strategia strategia ktora chcemy grac
#'@param powtorzenia liczba powtorzen gier do stestowania, domyslnie 100
#'@param stado_max maksymalny stan stada glownego
#'@param kostka_zielona zwierzeta na kostce zielonej
#'@param kostka_czerwona zwierzeta na kostce czerwonej
#'@param warunek_wygranej warunki konieczne do zakonczenia gry
#'@param co_zostawia_lis wektor zawierajacy liczbe zwierzat ktora maksymalnie zostaje w stadzie po ataku lisa
#'@param co_zostawia_wilk wektor zawierajacy liczbe zwierzat ktora maksymalnie zostaje w stadzie po ataku wilka
#'@param wartosc_w_krolikach wektor zawierajacy wartosci w krolikach poszczegolnych zwierzat
#'
#'@return wektor zawierajacy dane dotyczace dlugosci trwania wybranej liczby gier
#'
#'@examples
#'badaj_gre(strategia_owce)
#'warunki <- c('krolik'=3,"owca"=1,"swinia"=0,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=0)
#'badaj_gre(strategia_owce,warunek_wygranej=warunki)
#'@export

badaj_gre <- function(strategia,powtorzenia=100,stado_max=SuperFarmer.SuperDziewczyn::stado_max,kostka_zielona=SuperFarmer.SuperDziewczyn::kostka_zielona,kostka_czerwona=SuperFarmer.SuperDziewczyn::kostka_czerwona,warunek_wygranej=SuperFarmer.SuperDziewczyn::warunek_wygranej,co_zostawia_lis=SuperFarmer.SuperDziewczyn::co_zostawia_lis,co_zostawia_wilk=SuperFarmer.SuperDziewczyn::co_zostawia_wilk,wartosc_w_krolikach=SuperFarmer.SuperDziewczyn::wartosc_w_krolikach){
  czasy <- c()
  for (i in 1:powtorzenia){
    czasy[i] <- gra(strategia,stado_max,kostka_zielona,kostka_czerwona,warunek_wygranej,co_zostawia_lis,co_zostawia_wilk,wartosc_w_krolikach)[[2]]
  }

  return(czasy)
}
