#'Funkcja sprawdzajaca czy spelnione sa warunki wygranej
#'
#'Funkcja `wygrana()` przyjmuje wektor warunki wygranej oraz tabele zawierajaca stan stada gracza. Sprawdza ona czy nasza tabela zawiera wiecej lub tyle samo zwierzat danego rodzaju co warunek wygranej. Zwraca TRUE lub FALSE.
#'
#'@param warunek_wygranej wektor z warunkami zakonczenia gry
#'@param table stan stada gracza
#'
#'@return wartosc logiczna TRUE lub FALSE w zaleznosci od tego czy nasze stado spelnia warunki wygranej czy nie
#'
#'@examples
#'warunki_wygranej <- c("krolik"=1,"owca"=1,"swinia"=1,"krowa"=1,"kon"=1,"maly_pies"=0,"duzy_pies"=0)
#'stado <- c("krolik"=0,"owca"=1,"swinia"=1,"krowa"=1,"kon"=1,"maly_pies"=0,"duzy_pies"=0)
#'wygrana(warunki_wygranej,stado)


.wygrana <- function(warunek_wygranej,table){
  for(i in 1:7){
    if(table[[i]]<warunek_wygranej[[i]]) return(FALSE)}
  return(TRUE)
  }


