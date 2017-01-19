#'Wizytowka do strategii z Pakietu SuperFarmer.SuperDziewczyn.
#'
#'Funkcja wizytowka generuje wizytowke w formacie pdf opisujaca i wizualizujaca wybrana strategie.
#'
#'@param strategia Wybrana strategia, dla ktorej wizytowke chcemy uzyskac.
#'@param liczba_powtorzen Liczba wywolan gry celem uzyskania statystyk.
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
#'
#'@examples
#'\dontrun{
#'wizytowka_strategia_owce <- wizytowka(SuperFarmer.SuperDziewczyn::strategia_owce)
#'}
#'@export


wizytowka <- function(strategia,liczba_powtorzen = 1000){
  #dane do wizytowki
  przebieg_gry <- SuperFarmer.SuperDziewczyn::gra(strategia)
  macierz_przebiegu_gry <- przebieg_gry[[1]]

  przebieg_gier_superdziewczyn <- SuperFarmer.SuperDziewczyn::badaj_gre(strategia,powtorzenia = liczba_powtorzen)
  przebieg_gier_moc <- SuperFarmerMoc::badaj_gre(SuperFarmerMoc::strategia_postMDiPR,liczba_prob=liczba_powtorzen)
  przebieg_gier_rcnk <- SuperFarmer.SuperDziewczyn::badaj_gre(SuperFarmerRCNK::strategia_anty_yolo,powtorzenia=liczba_powtorzen)

  #jako ramki danych
  macierz_przebiegu_gry<-cbind(macierz_przebiegu_gry,c(1:przebieg_gry[[2]]))
  colnames(macierz_przebiegu_gry)[8] <- "Numer_kolejki"
  przebieg_gry <- as.data.frame(macierz_przebiegu_gry)

  #przygotowanie danych
  superdziewczyny <- gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))
  przebieg_gier_superdziewczyn <- przygotuj_dane(przebieg_gier_superdziewczyn, superdziewczyny, liczba_powtorzen)
  moc <- "strategia_postMDiPR"
  przebieg_gier_moc <- przygotuj_dane(przebieg_gier_moc, moc, liczba_powtorzen)
  rcnk <- "strategia_anty_yolo"
  przebieg_gier_rcnk <- przygotuj_dane(przebieg_gier_rcnk, rcnk, liczba_powtorzen)
  przebieg <- rbind(przebieg_gier_superdziewczyn,przebieg_gier_moc)
  przebieg <- rbind(przebieg,przebieg_gier_rcnk)

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
  statystyki <- tabela_statystyk(przebieg_gier_superdziewczyn,przebieg_gier_moc,przebieg_gier_rcnk,nazwa_strat)

  #decyle
  decyle <- tabela_decyli(przebieg_gier_superdziewczyn,przebieg_gier_moc,przebieg_gier_rcnk,nazwa_strat)

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
  G <- arrangeGrob(grobs=list(tytul,wykres_gestosc,autorzy, tekst,statystyki_tabela,decyle_tabela,owce_i_kroliki,swinki_krowy_koniki),layout_matrix=lay)

  #zapisywanie do pliku
  zapisz(grid.arrange(G),nazwa_strat)
}
