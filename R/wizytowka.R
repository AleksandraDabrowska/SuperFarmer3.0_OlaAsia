#'Wizytowka do strategii z Pakietu SuperFarmer.SuperDziewczyn
#'
#'Funkcja wizytowka generuje wizytowke w formacie pdf opisujaca wybrana strategie.
#'
#'@param strategia wybrana strategia
#'
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 geom_density
#'@importFrom ggplot2 geom_vline
#'@importFrom ggplot2 ggtitle
#'@importFrom ggplot2 scale_color_manual
#'@importFrom ggplot2 scale_fill_manual
#'@importFrom ggplot2 xlab
#'@importFrom ggplot2 ylab
#'@importFrom ggplot2 ggsave
#'@importFrom ggplot2 ylim
#'@importFrom ggplot2 theme
#'@importFrom ggplot2 element_rect
#'@importFrom ggplot2 labs
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
#'
#'@importFrom stats sd
#'@importFrom stats median
#'@importFrom optimbase transpose
#'
#'@importFrom grDevices dev.off
#'@importFrom grDevices pdf
#'
#'
#'
#'@examples
#'\dontrun{
#'wizytowka_strategia_owce <- wizytowka(SuperFarmer.SuperDziewczyn::strategia_owce)
#'}
#'@export


wizytowka <- function(strategia){
  #dane do wizytowki

  przebieg_gry <- SuperFarmer.SuperDziewczyn::gra(SuperFarmer.SuperDziewczyn::strategia_owce)
  macierz_przebiegu_gry <- przebieg_gry[[1]]
  
  
  przebieg_100_gier_superdziewczyn <- SuperFarmer.SuperDziewczyn::badaj_gre(SuperFarmer.SuperDziewczyn::strategia_owce,powtorzenia = 100)
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
  
  przebieg_100_gier_superdziewczyn <-cbind(przebieg_100_gier_superdziewczyn, sample(deparse(substitute(strategia)),100, replace=TRUE))
  
  przebieg_100_gier_moc <-cbind(przebieg_100_gier_moc, sample("SuperFarmerMoc::strategia_postMDiPR",100, replace=TRUE))
  
  przebieg_100_gier_rcnk <-cbind(przebieg_100_gier_rcnk, sample("SuperFarmerRCNK::strategia_anty_yolo",100, replace=TRUE))
  
  colnames(przebieg_100_gier_superdziewczyn)[2] <- "Strategia"
  colnames(przebieg_100_gier_moc)[2] <- "Strategia"
  colnames(przebieg_100_gier_rcnk)[2] <- "Strategia"
  
  przebieg <- rbind(przebieg_100_gier_superdziewczyn,przebieg_100_gier_moc)
  przebieg <- rbind(przebieg,przebieg_100_gier_rcnk)
  
  srednia <-ddply(przebieg, "Strategia", summarise, grp.mean=mean(przebieg$Liczba_ruchow))
  
  mediana <- ddply(przebieg,"Strategia",summarise,grp.median=median(przebieg$Liczba_ruchow))
  #wykres gestosci dla najlepszej strategii z pakietu moc, najgorszej z rcnk i naszej - owce
  
  wykres_gestosc <- ggplot(przebieg, aes(przebieg$Liczba_ruchow,colour=przebieg$Strategia,fill=przebieg$Strategia))+
    geom_density(position="stack")+
    scale_color_manual(values=c("#2166ac","#1b7837","#762a83"))+
    scale_fill_manual(values=c("#d1e5f0","#d9f0d3","#e7d4e8"))+
    geom_vline(data=srednia, aes(xintercept=srednia$grp.mean, color=srednia$Strategia),linetype="dashed")+
    geom_vline(data=mediana, aes(xintercept=mediana$grp.median, color=mediana$Strategia))+
    ylab("Liczba gier")+
    xlab("Liczba ruchow")+
    ggtitle(paste0("Porownanie gestosci dla strategii ",deparse(substitute(strategia)),",\nSuperFarmerMoc::strategia_postMDiPR i SuperFarmerRCNK::strategia_anty_yolo"))+
    theme(panel.background = element_rect(fill="white"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20,angle = 0),
          title = element_text(size=25),
          legend.title = element_text(size=25),
          legend.text = element_text(size=20))+
    labs(fill="Strategia\n",color="Strategia\n")
  
  
  
  #wykres zwierzatka
  kroliki <- przebieg_gry[,c(1,8)]
  kroliki <- cbind(kroliki,sample("krolik",nrow(kroliki),replace=TRUE))
  colnames(kroliki)[3] <- "zwierze"
  colnames(kroliki)[1] <- "liczba"
  owce <- przebieg_gry[,c(2,8)]
  owce <- cbind(owce,sample("owca",nrow(owce),replace=TRUE))
  colnames(owce)[3] <- "zwierze"
  colnames(owce)[1] <- "liczba"
  
  zwierzatka <- rbind(kroliki,owce)
  
  #teraz wykres pokazujacy rozlozenie zwierzatek w stadzie dla kazdej kolejki
  
  owce_i_kroliki <- ggplot(zwierzatka,aes(zwierzatka$Numer_kolejki,zwierzatka$liczba,col=zwierzatka$zwierze,shape=zwierzatka$zwierze))+
    geom_point(size=3)+
    ylab("Liczba zwierzatek")+
    xlab("Numer kolejki")+
    ggtitle("Liczba krolikow i owiec w pojedynczej grze")+
    theme(panel.background = element_rect(fill="white"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20,angle = 0),
          title = element_text(size=30),
          legend.text = element_text(size=20),
          legend.title = element_text(size=25))+
    labs(color="Zwierze\n",shape="Zwierze\n")
    
    
  #wykres konie, krowy swinki
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

  swinki_krowy_koniki <- ggplot(zwierzatka_duze,aes(zwierzatka_duze$Numer_kolejki,zwierzatka_duze$liczba,col=zwierzatka_duze$zwierze,shape=zwierzatka_duze$zwierze))+
    geom_point(size=3)+
    ylab("Liczba zwierzatek")+
    xlab("Numer kolejki")+
    ggtitle("Liczba swinek, krow i konikow w pojedynczej grze")+
    theme(panel.background = element_rect(fill="white"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20,angle = 0),
          title = element_text(size=30),
          legend.text = element_text(size=20),
          legend.title = element_text(size=25))+
    labs(color="Zwierze\n",shape="Zwierze\n")
  
    
  
  #tytul
  tytul <- textGrob(paste0("Wizytowka ",deparse(substitute(strategia)),"\n  Aleksandra Dabrowska, Joanna Zbijewska"),gp=gpar(fontsize=35, col="black"))
  
  
  #to co chcemy dolozyc jako tekst
  tekst <- textGrob((paste0("\nPrzedstawiamy ",deparse(substitute(strategia))," z pakietu SuperFarmer.SuperDziewczyn.\n Porownalysmy ja ze strategia strategia_postMDiPR z pakietu SuperFarmerMoc, dajaca najlepsze wyniki \n oraz strategia strategia_anty_yolo z pakietu SuperFarmerRCNK, ktora dawala najdluzsze czasy gry.\n Porownanie przedstawilysmy na wykresie gestosci,na ktorym dodatkowo zaznaczane sa srednia i mediana\n dla kazdej strategii, a takze w tabeli z podstawowymi statystykami. Jednoczesnie dla przedstawionej strategii \n przedstawiamy zmiany liczby niektorych zwierzat w stadzie podczas pojedynczej gry.")),gp=gpar(fontsize=22, col="black"))
  
  #statystyki na wczesniej przygotowanych danych
  
  minimum <- c(min(przebieg_100_gier_superdziewczyn$Liczba_ruchow),min(przebieg_100_gier_moc$Liczba_ruchow),min(przebieg_100_gier_rcnk$Liczba_ruchow))
  
  maksimum <- c(max(przebieg_100_gier_superdziewczyn$Liczba_ruchow),max(przebieg_100_gier_moc$Liczba_ruchow),max(przebieg_100_gier_rcnk$Liczba_ruchow))
  
  odchylenie <- c(sd(przebieg_100_gier_superdziewczyn$Liczba_ruchow),sd(przebieg_100_gier_moc$Liczba_ruchow),sd(przebieg_100_gier_rcnk$Liczba_ruchow))
  odchylenie <- round(odchylenie, digits = 2)
  
  srednia_zwykla <- c(mean(przebieg_100_gier_superdziewczyn$Liczba_ruchow),mean(przebieg_100_gier_moc$Liczba_ruchow),mean(przebieg_100_gier_rcnk$Liczba_ruchow))
  
  srednia_odcieta <- c(mean(przebieg_100_gier_superdziewczyn$Liczba_ruchow,trim=0.2),mean(przebieg_100_gier_moc$Liczba_ruchow,trim=0.2),mean(przebieg_100_gier_rcnk$Liczba_ruchow,trim=0.2))
  
  srednia_odcieta <- round(srednia_odcieta, digits = 2)
  
  
  mediana_zwykla <- c(median(przebieg_100_gier_superdziewczyn$Liczba_ruchow),median(przebieg_100_gier_moc$Liczba_ruchow),median(przebieg_100_gier_rcnk$Liczba_ruchow))
  
  rozrzut <- maksimum-minimum
  
  statystyki <- rbind(minimum,maksimum)
  statystyki <- rbind(statystyki,rozrzut)
  statystyki <- rbind(statystyki,odchylenie)
  statystyki <- rbind(statystyki,srednia_zwykla)
  statystyki <- rbind(statystyki,srednia_odcieta)
  statystyki <- rbind(statystyki,mediana_zwykla)
 
  statystyki <- t(statystyki)
  statystyki <- as.data.frame(statystyki)
  
  rownames(statystyki) <- c(deparse(substitute(strategia)),"SuperFarmerMoc::strategia_postMDiPR","SuperFarmerRCNK::strategia_anty_yolo")
  colnames(statystyki)<-c("minimum","maksimum","rozrzut","odchylenie\n standardowe","srednia","srednia odcieta","mediana")

  
 
  
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 2.0),bg_params = list(fill = c("#d1e5f0","#d9f0d3","#e7d4e8"))),
    colhead = list(fg_params=list(cex = 2.0)),
    rowhead = list(fg_params=list(cex = 1.2)))
  
  statystyki_tabela <- tableGrob(statystyki, theme = mytheme)
  #statystyki_tabela$widths <- unit(rep(1/ncol(statystyki), ncol(statystyki)), "npc")
  #statystyki_tabela$heights <-unit(rep(2/nrow(statystyki), nrow(statystyki)), "npc")

  
  #ustawienia na stronie 
  
  layout <- rbind(c(1,1,1,1,2,2,2,2),
                  c(3,3,3,3,2,2,2,2),
                  c(3,3,3,3,2,2,2,2),
                  c(3,3,3,3,4,4,4,4),
                  c(5,5,5,5,6,6,6,6),
                  c(5,5,5,5,6,6,6,6))
  
  lay <- rbind(c(1,2),
               c(3,2),
               c(3,2),
               c(4,4),
               c(5,6),
               c(5,6))
  
  #oba <-gridExtra::grid.arrange(grobs=c(tytul,wykres_gestosc, tekst,statystyki_tabela,owce_i_kroliki,swinki_krowy_koniki),layout_matrix=lay) #na rownych skalach
  pdf("asia.pdf", width = 29.7, height = 21) # Open a new pdf file
  grid.arrange(tytul,tekst,wykres_gestosc,statystyki_tabela,owce_i_kroliki,swinki_krowy_koniki,nrow=3,ncol=2,widths=c(14.8,14.8),heights=c(5, 8, 8))
  dev.off()
  
  
}