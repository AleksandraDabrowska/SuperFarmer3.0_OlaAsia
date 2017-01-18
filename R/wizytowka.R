#'Wizytowka do strategii z Pakietu SuperFarmer.SuperDziewczyn.
#'
#'Funkcja wizytowka generuje wizytowke w formacie pdf opisujaca i wizualizujaca wybrana strategie.
#'
#'@param strategia Wybrana strategia, dla ktorej wizytowke chcemy uzyskac.
#'
#'
#'@importFrom plyr ddply
#'@importFrom plyr summarise
#'
#'@importFrom grid textGrob
#'@importFrom grid gpar
#'@importFrom grid unit 
#'
#'@importFrom gridExtra grid.arrange
#'@importFrom gridExtra tableGrob
#'@importFrom gridExtra ttheme_default
#'@importFrom gridExtra arrangeGrob
#'
#'@importFrom grDevices dev.off
#'@importFrom grDevices pdf
#'
#'
#'@examples
#'\dontrun{
#'wizytowka_strategia_owce <- wizytowka(SuperFarmer.SuperDziewczyn::strategia_owce)
#'}
#'@export


wizytowka <- function(strategia){
  #dane do wizytowki
  przebieg_gry <- SuperFarmer.SuperDziewczyn::gra(strategia)
  macierz_przebiegu_gry <- przebieg_gry[[1]]
  
  przebieg_100_gier_superdziewczyn <- SuperFarmer.SuperDziewczyn::badaj_gre(strategia,powtorzenia = 100)
  przebieg_100_gier_moc <- SuperFarmerMoc::badaj_gre(SuperFarmerMoc::strategia_postMDiPR,liczba_prob=100)
  przebieg_100_gier_rcnk <- SuperFarmer.SuperDziewczyn::badaj_gre(SuperFarmerRCNK::strategia_anty_yolo,powtorzenia=100)

  #jako ramki danych
  macierz_przebiegu_gry<-cbind(macierz_przebiegu_gry,c(1:przebieg_gry[[2]]))
  colnames(macierz_przebiegu_gry)[8] <- "Numer_kolejki"
  przebieg_gry <- as.data.frame(macierz_przebiegu_gry)
  
  przebieg_100_gier_superdziewczyn <- as.data.frame(przebieg_100_gier_superdziewczyn)
  przebieg_100_gier_moc <- as.data.frame(przebieg_100_gier_moc)
  przebieg_100_gier_rcnk <- as.data.frame(przebieg_100_gier_rcnk)
  colnames(przebieg_100_gier_superdziewczyn) <- "Liczba_ruchow"
  colnames(przebieg_100_gier_moc) <- "Liczba_ruchow"
  colnames(przebieg_100_gier_rcnk) <- "Liczba_ruchow"
  przebieg_100_gier_superdziewczyn <-cbind(przebieg_100_gier_superdziewczyn, sample(gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))),100, replace=TRUE))
  przebieg_100_gier_moc <-cbind(przebieg_100_gier_moc, sample("strategia_postMDiPR",100, replace=TRUE))
  przebieg_100_gier_rcnk <-cbind(przebieg_100_gier_rcnk, sample("strategia_anty_yolo",100, replace=TRUE))
  colnames(przebieg_100_gier_superdziewczyn)[2] <- "Strategia"
  colnames(przebieg_100_gier_moc)[2] <- "Strategia"
  colnames(przebieg_100_gier_rcnk)[2] <- "Strategia"
  
  przebieg <- rbind(przebieg_100_gier_superdziewczyn,przebieg_100_gier_moc)
  przebieg <- rbind(przebieg,przebieg_100_gier_rcnk)
  
  srednia <-ddply(przebieg, "Strategia", summarise, grp.mean=mean(Liczba_ruchow))
  mediana_wykres <- ddply(przebieg,"Strategia",summarise,grp.median=median(Liczba_ruchow))
  
  #wykres gestosci dla najlepszej strategii z pakietu moc, najgorszej z rcnk i naszej
  nazwa <- gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))
  wykres_gestosc <- SuperFarmer.SuperDziewczyn::wykres_gestosci(przebieg,nazwa,mediana_wykres,srednia)
  
  #dane do wykresu owiec i krolikow
  kroliki <- przebieg_gry[,c(1,8)]
  kroliki <- cbind(kroliki,sample("krolik",nrow(kroliki),replace=TRUE))
  colnames(kroliki)[3] <- "zwierze"
  colnames(kroliki)[1] <- "liczba"
  owce <- przebieg_gry[,c(2,8)]
  owce <- cbind(owce,sample("owca",nrow(owce),replace=TRUE))
  colnames(owce)[3] <- "zwierze"
  colnames(owce)[1] <- "liczba"

  zwierzatka <- rbind(kroliki,owce)
  
  #teraz wykres pokazujacy rozlozenie owiec i krolikow w stadzie dla kazdej kolejki
  tytul1 <- "Liczba krolikow i owiec w pojedynczej grze"
  owce_i_kroliki <- wykres_zwierzat(zwierzatka,tytul1)
  
  #dane do wykresu konie, krowy i swinki
  swinki <- przebieg_gry[,c(3,8)]
  swinki <- cbind(swinki,sample("swinia",nrow(swinki),replace=TRUE))
  colnames(swinki)[3] <- "zwierze"
  colnames(swinki)[1] <- "liczba"
  krowy <- przebieg_gry[,c(4,8)]
  krowy <- cbind(krowy,sample("krowa",nrow(krowy),replace=TRUE))
  colnames(krowy)[3] <- "zwierze"
  colnames(krowy)[1] <- "liczba"
  konie <- przebieg_gry[,c(5,8)]
  konie <- cbind(konie,sample("kon",nrow(konie),replace=TRUE))
  colnames(konie)[3] <- "zwierze"
  colnames(konie)[1] <- "liczba"
  
  zwierzatka_duze <- rbind(swinki,krowy)
  zwierzatka_duze <- rbind(zwierzatka_duze,konie)

  #teraz wykres dla liczebnosci koni, krow i swin dla kazdej kolejki
  tytul2 <- "Liczba swinek, krow i konikow w pojedynczej grze"
  swinki_krowy_koniki <- wykres_zwierzat(zwierzatka_duze,tytul2)
  
  #tytul
  tytul <- textGrob(paste0("Wizytowka\n SuperFarmer.SuperDziewczyn\n ",gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))),gp=gpar(fontsize=48, col="black", fontface = "bold"))
  
  #wykonanie
  autorzy <- textGrob("Aleksandra Dabrowska,\n Joanna Zbijewska",gp=gpar(fontsize=40, col="black"))
  
  #to co chcemy dolozyc jako tekst
  tekst <- textGrob((paste0("\nPrzedstawiamy ",gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))),"\n z pakietu SuperFarmer.SuperDziewczyn.\n Porownalysmy ja ze strategia strategia_postMDiPR \n z pakietu SuperFarmerMoc, dajaca najlepsze wyniki \n oraz strategia strategia_anty_yolo \n z pakietu SuperFarmerRCNK, ktora dawala najdluzsze gry.\n Porownanie przedstawilysmy na wykresie gestosci, \n na ktorym zaznaczone sa srednia i mediana dla kazdej strategii,\n a takze w tabeli z podstawowymi statystykami. \n Jednoczesnie dla przedstawionej strategii \n przedstawiamy zmiany liczby niektorych zwierzat w stadzie \n podczas pojedynczej gry.")),gp=gpar(fontsize=25, col="black"))
  
  #statystyki na wczesniej przygotowanych danych
  nazwa_strat <- gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))
  statystyki <- tabela_statystyk(przebieg_100_gier_superdziewczyn,przebieg_100_gier_moc,przebieg_100_gier_rcnk,nazwa_strat)
  
  #decyle
  decyle <- tabela_decyli(przebieg_100_gier_superdziewczyn,przebieg_100_gier_moc,przebieg_100_gier_rcnk,nazwa_strat)
 
  #wyglad tabeli
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1.8),bg_params = list(fill = c("#d1e5f0","#d9f0d3","#e7d4e8"))),
    colhead = list(fg_params=list(cex = 1.8)),
    rowhead = list(fg_params=list(cex = 1.9, fontface = "bold")))
  
  statystyki_tabela <- tableGrob(statystyki, theme = mytheme)
  decyle_tabela <- tableGrob(decyle,theme=mytheme)
  
  #ustawienia na stronie 
  lay <- rbind(c(1,1,2,2,2,2),
               c(3,3,2,2,2,2),
               c(4,4,2,2,2,2),
               c(4,4,5,5,5,5),
               c(4,4,6,6,6,6),
               c(7,7,7,8,8,8),
               c(7,7,7,8,8,8))
  
  #zapisywanie do pliku
  G <- arrangeGrob(grobs=list(tytul,wykres_gestosc,autorzy, tekst,statystyki_tabela,decyle_tabela,owce_i_kroliki,swinki_krowy_koniki),layout_matrix=lay) #na rownych skalach
  pdf(paste0(gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))),".pdf"), width = 29.7, height = 21) # Open a new pdf file
  grid.arrange(G)
  dev.off()
  
}