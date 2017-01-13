#'Funkcja opisujaca przebieg rzutu i zwiazane z nim zmiany w stadzie gracza i stadzie glownym
#'
#'Funkcja `przebieg_rzutu()` przyjmuje 6 argumentow. Symuluje ona jednorazowy rzut kostka przez gracza oraz zwiazane tylko z tym rzutem zmiany w stadzie gracza. Zwraca ona nowy stan stada gracza.
#'
#'@param stado stan stada gracza
#'@param stado_max maksymalna wartosc stada glownego
#'@param kostka_zielona zwierzeta na kostce zielonej
#'@param  kostka_czerwona zwierzeta na kostce czerwonej
#'@param co_zostawia_lis wektor zwierzat ktore zostawia lis po ataku, jeśli nie bylo malego psa w stadzie gracza
#'@param co_zostawia_wilk wektor zwierzat ktore zostawia wilk po ataku, jeśli nie bylo malego psa w stadzie gracza
#'
#'@return stado stan stada gracza
#'
#'@examples
#'stado_gracza <- c("krolik"=1,"owca"=2,"swinia"=2,"krowa"=2,"kon"=0,"maly_pies"=0,"duzy_pies"=0)
#'stado_max <- c(60,24,20,12,6,4,2)
#'names(stado_max) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
#'kostka_czerwona <- c("krolik","krolik","krolik","krolik", "krolik","krolik","owca","owca",
#'"swinia","swinia","kon","lis")
#'kostka_zielona <- c("krolik","krolik","krolik","krolik", "krolik","krolik","owca","owca",
#'"owca","swinia","krowa","wilk")
#'przebieg_rzutu(stado_gracza,stado_max,kostka_zielona,kostka_czerwona)



.przebieg_rzutu <- function(stado,stado_max,kostka_zielona,kostka_czerwona,co_zostawia_lis,co_zostawia_wilk){
  kostki<-.rzut_kostkami(kostka_zielona,kostka_czerwona)
  czy_wilk <- 'wilk' %in% kostki
  czy_lis <- 'lis' %in% kostki
  if (czy_wilk | czy_lis){
    stado <- .atak_drapieznika(czy_lis,czy_wilk,stado,stado_max,co_zostawia_lis,co_zostawia_wilk)
  }

  wylosowane <- .zamien_wynik_rzutu_na_wektor(kostki)
  names(wylosowane) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")

  otrzymane <- numeric(7)
  names(otrzymane) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
  indeksy <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
  #cos tu jest nie tak ale na razie nie moge okreslic co
  for(i in indeksy){
    if (wylosowane[[i]] == 0){
      otrzymane[[i]] <- 0
    } else{
      otrzymane[[i]] <- trunc((stado[[i]]+wylosowane[[i]])/2)
    }
  }
  otrzymane <- .dostaniemy(otrzymane,stado,stado_max-stado)
  if (!all(otrzymane==0)){
    stado <- .dodaj_lub_odejmij_zwierzeta(otrzymane,1,stado,stado_max-stado)
  }

  return(stado)
}
