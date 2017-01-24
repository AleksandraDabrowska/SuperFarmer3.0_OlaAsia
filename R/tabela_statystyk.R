#' Generowanie tabeli z podstawowymi statystykami dla trzech strategii
#'
#' Funkcja tabela_statystyk() liczy podstawowe statystyki dla trzech strategii i przedstawia je w tabeli.
#'
#' @param dane1 Ramka danych z liczba ruchow dla gier z pierwsza strategia.
#' @param dane2 Ramka danych z liczba ruchow dla gier z druga strategia.
#' @param dane3 Ramka danych z liczba ruchow dla gier z trzecia strategia.
#' @param strat Nazwa strategii pierwszej.
#'
#' @importFrom stats sd
#' @importFrom stats median
#'
#' @examples
#'\dontrun{
#'tabela_stat <- tabela_statystyk(superdziewczyn,moc,rcnk,SuperFarmer.SuperDziewczyn::strategia_owce)
#'}
#'
#' @seealso tabela_decyli
#'
#' @export

tabela_statystyk <- function(dane1, dane2, dane3, strat){
  minimum <- c(min(dane1$Liczba_ruchow),min(dane2$Liczba_ruchow),min(dane3$Liczba_ruchow))
  maksimum <- c(max(dane1$Liczba_ruchow),max(dane2$Liczba_ruchow),max(dane3$Liczba_ruchow))
  odchylenie <- c(sd(dane1$Liczba_ruchow),sd(dane2$Liczba_ruchow),sd(dane3$Liczba_ruchow))
  odchylenie <- round(odchylenie, digits = 2)
  srednia_zwykla <- c(mean(dane1$Liczba_ruchow),mean(dane2$Liczba_ruchow),mean(dane3$Liczba_ruchow))
  srednia_odcieta <- c(mean(dane1$Liczba_ruchow,trim=0.2),mean(dane2$Liczba_ruchow,trim=0.2),mean(dane3$Liczba_ruchow,trim=0.2))
  srednia_odcieta <- round(srednia_odcieta, digits = 2)
  mediana <- c(median(dane1$Liczba_ruchow),median(dane2$Liczba_ruchow),median(dane3$Liczba_ruchow))


  rozrzut <- maksimum-minimum
  odchylenie <- round(odchylenie, 0)
  srednia_zwykla <- round(srednia_zwykla, 0)
  srednia_odcieta <- round(srednia_odcieta,0)
  mediana <- round(mediana,0)

  statystyki <- rbind(minimum,maksimum)
  statystyki <- rbind(statystyki,rozrzut)
  statystyki <- rbind(statystyki,odchylenie)
  statystyki <- rbind(statystyki,srednia_zwykla)
  statystyki <- rbind(statystyki,srednia_odcieta)
  statystyki <- rbind(statystyki,mediana)

  statystyki <- t(statystyki)
  statystyki <- as.data.frame(statystyki)

  rownames(statystyki) <- c(strat,"strategia_postMDiPR","strategia_anty_yolo")
  colnames(statystyki)<-c("minimum","maksimum","rozrzut","odchylenie\n standardowe","średnia","średnia odcięta","mediana")

  return(statystyki)
}
