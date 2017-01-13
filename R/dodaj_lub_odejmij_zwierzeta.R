 #'Funkcja zmieniajaca stan stad: gracza i glownego
 #'
 #'Funkcja `dodaj_lub_odejmij_zwierzeta()` przyjmuje 4 argumenty. W zaleznosci od argumentu `jak` dodaje lub odejmuje od stada gracza zadana jako argument tabele/nazwany wektor. Zwraca stan stada gracza.
#'
#'@param co mozliwe zmiany w stadzie
#'@param jak 1:dodawanie do stada gracza, -1:odejmowanie od stada gracza
#'@param stado stan stada gracza
#'@param stado_glowne stan stada glownego
#'
#'@return stan stada gracza po zmianach
#'
#'@seealso `dokonanie_wymian`
#'
#'@examples
#'stado <- c("krolik"=3,"owca"=1,"swinia"=3,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=1)
#'tabela <- c("krolik"=1,"owca"=1,"swinia"=0,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=0)
#'stado_glowne <- c(60,24,20,12,6,4,2)
#'names(stado_glowne) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
#'dodaj_lub_odejmij_zwierzeta(tabela,1,stado,stado_glowne)
#'co <- c("krolik"=1,"owca"=1,"swinia"=0,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=0)
#'dodaj_lub_odejmij_zwierzeta(co,-1,stado,stado_glowne)


.dodaj_lub_odejmij_zwierzeta <- function(co, jak=1,stado,stado_glowne){
  stado <- stado + jak * co
  stado_glowne <- stado_glowne - jak * co
  return(stado)
}
