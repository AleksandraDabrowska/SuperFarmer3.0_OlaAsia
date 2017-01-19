#' Zwraca tabele z decylami dla strategii.
#' 
#' Funkcja tabela_decyli oblicza decyle liczby ruchow dla trzech strategii i zwraca je w tabeli.
#' 
#' @param dane1 Ramka danych z liczba ruchow dla gier z pierwsza strategia.
#' @param dane2 Ramka danych z liczba ruchow dla gier z druga strategia.
#' @param dane3 Ramka danych z liczba ruchow dla gier z trzecia strategia.
#' @param strat Nazwa strategii pierwszej.
#'
#' @importFrom stats quantile
#' 
#' @export

tabela_decyli <- function(dane1, dane2, dane3, strat){
  decyle_superfarmer <- quantile(dane1$Liczba_ruchow,prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  decyle_moc <- quantile(dane2$Liczba_ruchow,prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  decyle_rcnk <- quantile(dane3$Liczba_ruchow,prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  
  decyle_superfarmer <- round(decyle_superfarmer)
  deycle_moc <- round(decyle_moc)
  decyle_rcnk <- round(decyle_rcnk)
  
  decyle <- rbind(decyle_superfarmer,decyle_moc)
  decyle <- rbind(decyle, decyle_rcnk)
  
  

  decyle <- as.data.frame(decyle)
  
  rownames(decyle) <- c(strat,"strategia_postMDiPR","strategia_anty_yolo")
  
  return(decyle)
}