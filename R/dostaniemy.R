#'Funkcja wyznaczajaca ile zwierzat mozemy otrzymac ze stada
#'
#'Funkcja `dostaniemy()` przyjmuje 3 argumenty. W zaleznosci od argumentu tyle_chce_wziac sprawdza czy możliwe jest uzyskanie ze stada glownego podanej liczby zwierzat.
#'
#'@param tyle_chce_wziac Te zwierzeta chcemy otrzymac
#'@param stado Stan stada gracza
#'@param stado_glowne Stan stada glownego
#'
#'@return tyle_chce_wziac tabela zawierajaca liczby zwierzat mozliwe do uzyskania ze stada glownego
#'
#'@examples
#'chcemy <- c(1,1,1,0,0,0,0)
#'stado <- c("krolik"=3,"owca"=1,"swinia"=3,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=1)
#'stado_max <- c(60,24,20,12,6,4,2)
#'names(stado_max) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
#'dostaniemy(chcemy,stado,stado_max-stado)



.dostaniemy <- function(tyle_chce_wziac,stado,stado_glowne){
  tyle_chce_wziac <- ifelse(tyle_chce_wziac <= stado_glowne, tyle_chce_wziac, stado_glowne)
  return(tyle_chce_wziac)
}
