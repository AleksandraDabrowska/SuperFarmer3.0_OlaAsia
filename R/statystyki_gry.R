#'Funkcja obliczajaca podstawowe statystki dla wybranej wczesniej liczby gier
#'
#'Funkcja`statystyki_gry()` przyjmuje wektor dlugosci n zawierajacy informacje o dlugosci poszczegolnych gier. Zwraca podstawowe statystyki (min,max,kwantyle,mediana,srednia) dotyczace dlugosci trwania gier.
#'
#'@param wektor wektor zawierajacy dane dotyczace dlugosci trwania poszczegolnych gier
#'
#'@return statystyki dla gry
#'
#'@examples
#'wektor <- c(20,40,20,10,12,55)
#'statystyki_gry(wektor)
#'
#'@export

statystyki_gry <- function(wektor){
  czasy <- as.data.frame(wektor)
  colnames(czasy) <- "Czas_gry"
  statystyki <- summary(czasy$Czas_gry)

  return(statystyki)
}
