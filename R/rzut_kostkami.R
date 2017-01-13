#'Wynik rzutu dwoma kostkami
#'
#'Funkcja `rzut_kostkami()` przyjmuje dwa argumenty - nazwy kostek .Wyznacza pare wartosci - nazwy zwierzat na obu kostkach.
#'
#'@param a wektor zawierajacy zwierzeta na kostce zielonej
#'@param b wektor zawierajacy zwierzeta na kostce czerwonej
#'
#'@return Wektor zawierajacy nazwy zwierzat wylosowanych na kostce zielonej i czerwonej odpowiednio
#'
#'@examples
#'kostka_czerwona <- c("krolik","krolik","krolik","krolik", "krolik","krolik","owca","owca",
#'"swinia","swinia","kon","lis")
#'kostka_zielona <- c("krolik","krolik","krolik","krolik", "krolik","krolik","owca","owca",
#'"owca","swinia","krowa","wilk")
#'rzut_kostkami(kostka_zielona,kostka_czerwona)


.rzut_kostkami <- function(a,b){
  wynik <- c(sample(a, 1), sample(b, 1))
  return(wynik)
}
