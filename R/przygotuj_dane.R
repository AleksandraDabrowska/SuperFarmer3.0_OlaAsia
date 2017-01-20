#' Przygotowanie danych z n wywolan gry.
#'
#' Funkcja przygotuj_dane() przygotowuje ramke daych z zadanego wektora z danymi.
#'
#' @param do_przygotowania Wektor, ktory chcemy przygotowac.
#' @param nazwa Nazwa dla parametru, ktore opisuje wektor wejsciowy.
#' @param n Liczba wywolan gry.
#'
#'\dontrun{
#'dane_moc <- przygotuj_dane(moc,"Strategia_moc",100)
#'}
#'
#' @export

przygotuj_dane <- function(do_przygotowania, nazwa, n){
  dane <- as.data.frame(do_przygotowania)
  colnames(dane) <- "Liczba_ruchow"
  dane <- cbind(dane, sample(nazwa,n, replace=TRUE))
  colnames(dane)[2] <- "Strategia"
  return(dane)
}
