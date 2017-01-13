#'Funkcja obliczajaca wartosc naszego stada
#'
#'Funkcja `stado_w_krolikach()` przyjmuje 2 argumenty. Oblicza ona wartosc stada wyznaczona na podstawie liczebnosci stada i wartosci poszczegolnych zwierzat wyrazonej w krolikach.
#'
#'@param wartosc_w_krolikach wartosc danych zwierzat
#'@param tabela tabela dla ktorej chcemy liczyc wartosc
#'
#'@return saldo czyli wartosc tabeli
#'
#'@examples
#'tabela <- c("krolik"=2,"owca"=1,"swinia"=1,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=1)
#'wartosc_w_krolikach <- c(1,6,12,36,72,6,36)
#'names(wartosc_w_krolikach) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
#'stado_w_krolikach(wartosc_w_krolikach,tabela)


.stado_w_krolikach <- function(wartosc_w_krolikach,tabela){
  saldo <- 0
  for(i in 1:length(tabela)){
    saldo <- saldo + tabela[[i]]*wartosc_w_krolikach[[i]]
  }
  return(saldo)
}
