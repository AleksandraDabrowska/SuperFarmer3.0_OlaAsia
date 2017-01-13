#'Strategia wymian
#'
#'Funkcja `strategia_wymian_0_0_0_0()` otrzymuje jako argument stan stada gracza. Zaklada ona ze jesli nie jest mozliwa wymiana na konia to wymianiamy sie na mozliwie najwartosciowsze zwierze z listy. Po zdobyciu dwoch koni, wymieniamy jednego na zwierzeta pozostalych gatunkow.
#'
#'@param stado stado gracza
#'
#'@return wymiany ze strategii
#'
#'@seealso `strategia_owce` i `strategia_wymian_1_1_1_1`
#'
#'@export

strategia_wymian_0_0_0_0 <- function(stado){
  wymiana<-numeric(7)
  names(wymiana) <-c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
  names(stado) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')

  kroliki <- 0
  owce <-0
  male_psy <- 0
  duze_psy <-0

  wartosc_w_krolikach <- numeric(7)
  wartosc_w_krolikach <- c(1,6,12,36,72,6,36)
  names(wartosc_w_krolikach) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')


  if(stado[["kon"]]>= 2){
    wymiana[1:7]<-c(6,1,2,1,-1,0,0)
    stado <- .dokonanie_wymian(stado,wymiana)
    return(stado)
  }
  if((.stado_w_krolikach(wartosc_w_krolikach,stado[c(1,2,3,4,6,7)])>=72)&&(stado[["kon"]]==1)){
    wymiana <- .kilka_na_jedno(wartosc_w_krolikach,stado,"kon")
    stado <- .dokonanie_wymian(stado,wymiana)
    return(stado)
  }
  damy<-c( max(stado[["krolik"]]-kroliki,0),max(stado[["owca"]]-owce,0),stado["swinia"],stado["krowa"],0,max(stado[["maly_pies"]]-male_psy,0),max(stado[["duzy_pies"]]-duze_psy))
  names(damy) <-  c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
  for(zw in c("kon","maly_pies","duzy_pies","owca")){
    wymiana <-.kilka_na_jedno(wartosc_w_krolikach,damy,zw)
    param = switch(zw,"kon" = 10,
                   "maly_pies" = male_psy,
                   "duzy_pies" = duze_psy,
                   "owca" = owce)
    if(class(wymiana)=="numeric" && stado[[zw]]< param){
      stado <- .dokonanie_wymian(stado,wymiana)
      return(stado)
    }
  }
  return(stado)
}
