#'Funkcja wymian kilku zwierzat na jedno
#'
#'Funkcja `kilka_na_jedno()` otrzymuje nazwany wektor zawierajacy liczbe zwierzat ktore chcemy wymienic oraz nazwe zwierzecia ktore chcemy otrzymac. Dokonuje ona wymiany kilku (mniej warosciowych) zwierzat na jedno. Zwraca tabele zmian mozliwa do wykoniania.
#'
#'@param wartosc wartosc zwierzat wyrazona w krolikach
#'@param tabela_co tabela ze zwierzetami ktore chcemy oddac
#'@param na_co nazwa zwierzecia ktore chcemy dostac
#'
#'@return tabela zmian, ktore na dana chwile mozna wykonac
#'
#'@seealso `jedno_na_kilka`
#'
#'@examples
#'tabela_co <-c("krolik"=2,"owca"=1,"swinia"=1,"krowa"=0,"kon"=0,"maly_pies"=0,"duzy_pies"=0)
#'wartosc_w_krolikach <- c(1,6,12,36,72,6,36)
#'names(wartosc_w_krolikach) <- c("krolik","owca","swinia","krowa","kon","maly_pies","duzy_pies")
#'kilka_na_jedno(wartosc_w_krolikach,tabela_co,"krowa")


.kilka_na_jedno <- function(wartosc,tabela_co, na_co){
  tabela_zmian <- numeric(7)
  names(tabela_zmian) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
  cena <- wartosc[[na_co]]
  kolejnosc <- c("krowa","swinia","owca","krolik","maly_pies","duzy_pies","kon")

  for (i in kolejnosc){
    if(tabela_co[[i]]>0 && wartosc[[i]]<=cena && cena >0){
      tabela_zmian[[i]] <- -min(floor(cena/wartosc[[i]]), tabela_co[[i]])
      cena <- cena + tabela_zmian[[i]]*wartosc[[i]]
    }
  }
  tabela_zmian[[na_co]] <- 1
  tabela_zmian2 <- as.numeric(tabela_zmian)

  ifelse(sum(tabela_zmian2==0)==6 || cena >0 , return(FALSE), return(tabela_zmian))
}
