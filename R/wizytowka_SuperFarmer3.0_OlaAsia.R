#'Wizytowka do strategii z Pakietu SuperFarmer.SuperDziewczyn.
#'
#'Funkcja wizytowka() generuje wizytowke w formacie pdf opisujaca wybrana strategie z pakietu SuperFarmer.SuperDziewczyn. Oprocz wizualizacji w postaci wykresu gestosci dla 3 strategii oraz wykresu przebiegu jednej gry prezentujemy tabele statystyk i deycli dla trzech wybranych strategii.
#'
#'@param strategia Wybrana strategia, dla ktorej wizytowke chcemy uzyskac.
#'@param liczba_powtorzen Liczba wywolan gry celem uzyskania statystyk gier.
#'
#'
#'@importFrom plyr ddply
#'@importFrom plyr summarise
#'
#'@importFrom tidyr gather
#'
#'@importFrom grid textGrob
#'@importFrom grid gpar
#'@importFrom grid unit
#'
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


wizytowka_SuperFarmer3.0_OlaAsia <- function(strategia,liczba_powtorzen = 1000){
  #zapisujemy sciezke w ktorej jestesmy
  path <- paste0(getwd())
  #mamy folder ktory bedzie przechowywal wizytowki, wchodzimy do niego
  setwd(paste0(getwd(), "/inst"))
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
  
  Strategia <- c(gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))),"strategia_postMDiPR","strategia_anty_yolo")
  
  srednia_superfarmer <- mean(przebieg_gier_superdziewczyn$Liczba_ruchow)
  srednia_moc <- mean(przebieg_gier_moc$Liczba_ruchow)
  srednia_rcnk <- mean(przebieg_gier_rcnk$Liczba_ruchow)
  
  srednia <- rbind(srednia_superfarmer,srednia_moc)
  srednia <- rbind(srednia,srednia_rcnk)
  srednia <- as.data.frame(srednia)
  srednia <- cbind(srednia,Strategia)
  colnames(srednia)[1] <- "grp.mean"
  
  
  mediana_superfarmer <- median(przebieg_gier_superdziewczyn$Liczba_ruchow)
  mediana_moc <- median(przebieg_gier_moc$Liczba_ruchow)
  mediana_rcnk <- median(przebieg_gier_rcnk$Liczba_ruchow)
  
  mediana <- rbind(mediana_superfarmer,mediana_moc)
  mediana <- rbind(mediana,mediana_rcnk)
  mediana <- as.data.frame(mediana)
  mediana <- cbind(mediana,Strategia)
  colnames(mediana)[1] <- "grp.median"
  
  #wykres gestosci dla najlepszej strategii z pakietu moc, najgorszej z rcnk i naszej
  nazwa <- gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))
  wykres_gestosc <- SuperFarmer.SuperDziewczyn::wykres_gestosci(przebieg,nazwa,mediana,srednia)
  
  przebieg_gry_waski <- gather(przebieg_gry,"Numer_kolejki","Liczba",-8)
  colnames(przebieg_gry_waski)[2] <- "Zwierze"
 #wykres slupkowy dla liczebnosci poszczegolnych zwierzat w stadzie glownym
  tytul_zwierzeta <- "Pojedyncza gra: zmiana liczby poszczegolnych zwierzat"
  slupki <- SuperFarmer.SuperDziewczyn::wykres_wszystkie_zwierzatka(przebieg_gry_waski,tytul_zwierzeta)

  #tytul
  tytul <- textGrob(paste0("Wizytowka\n SuperFarmer.SuperDziewczyn\n ",gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))),gp=gpar(fontsize=48, col="black", fontface = "bold"))
  
  #wykonanie
  autorzy <- textGrob("Aleksandra Dabrowska,\n Joanna Zbijewska",gp=gpar(fontsize=40, col="black"))
  
  #to co chcemy dolozyc jako tekst
  tekst <- textGrob(paste0("Przedstawiamy ",gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))," z pakietu \n SuperFarmer.SuperDziewczyn. Porownalysmy ja ze strategia\n strategia_postMDiPR  z pakietu SuperFarmerMoc,\n dajaca najlepsze wyniki oraz strategia\n strategia_anty_yolo z pakietu SuperFarmerRCNK, \nktora dawala najdluzsze gry.\n Porownanie przedstawilysmy na wykresie gestosci,\n na ktorym zaznaczone sa srednia i mediana\n dla kazdej strategii, a takze\n w tabeli z podstawowymi statystykami.\n Jednoczesnie dla przedstawionej strategii\n przedstawiamy zmiany liczby zwierzat\n w stadzie podczas jednej losowej gry."),gp=gpar(fontsize=26, col="black"))
  
  #statystyki na wczesniej przygotowanych danych
  nazwa_strat <- paste0("SuperFarmer3.0_OlaAsia_wizytowka_",gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))))
  nazwa_strat_2 <- paste0(gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))))
  statystyki <- tabela_statystyk(przebieg_gier_superdziewczyn,przebieg_gier_moc,przebieg_gier_rcnk,nazwa_strat_2)
  
  
  #decyle
  decyle <- tabela_decyli(przebieg_gier_superdziewczyn,przebieg_gier_moc,przebieg_gier_rcnk,nazwa_strat_2)
  
  #wyglad tabeli
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1.8),bg_params = list(fill = c("#d1e5f0","#d9f0d3","#e7d4e8"))),
    colhead = list(fg_params=list(cex = 1.8)),
    rowhead = list(fg_params=list(cex = 1.8, fontface = "bold")))
  
  statystyki_tabela <- tableGrob(statystyki, theme = mytheme)
  
  decyle_tabela <- tableGrob(decyle,theme=mytheme)
  
  
  #ustawienia na stronie
  lay <- rbind(c(1,1,2,2,2,2),
               c(3,3,2,2,2,2),
               c(4,4,2,2,2,2),
               c(4,4,5,5,5,5),
               c(4,4,6,6,6,6),
               c(7,7,7,7,7,7),
               c(7,7,7,7,7,7))
  G <- arrangeGrob(grobs=list(tytul,wykres_gestosc,autorzy, tekst,statystyki_tabela,decyle_tabela,slupki),layout_matrix=lay)
  
  #zapisywanie do pliku
  zapisz(grid.arrange(G),nazwa_strat)
  
  #wracamy do pierwotnej sciezki
  setwd(path)
}