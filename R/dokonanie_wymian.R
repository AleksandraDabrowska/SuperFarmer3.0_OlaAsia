#'Dokonanie wymian zwiazanych ze strategia gracza
#'
#'Funkcja dokonanie_wymian() jako argumeny przyjmuje stan stada gracza oraz tabele zawierajaca liczbe zwierzat zwiazanych z wymianami jakie mozna dokonac w danym momencie. Zwraca stan stada gracza po tejze wymianie.
#'
#'@param stado stado gracza
#'@param tabela tabela zmian dostepnych po wymianie
#'
#'@return stan stada gracza
#'
#'@examples
#'tabela <- c("krolik"=1,"owca"=1,"swinia"=2,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=0)
#'stado <- c("krolik"=3,"owca"=1,"swinia"=3,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=1)
#'dokonanie_wymian(stado,tabela)


.dokonanie_wymian <- function(stado,tabela){
  stado <- stado + tabela
  return(stado)
}
