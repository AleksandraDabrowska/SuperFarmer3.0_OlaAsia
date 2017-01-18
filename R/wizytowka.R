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
#'@importFrom ggplot2 geom_text
#'
#'@importFrom ggthemes theme_tufte
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
  
  mediana <- ddply(przebieg,"Strategia",summarise,grp.median=median(Liczba_ruchow))
  #wykres gestosci dla najlepszej strategii z pakietu moc, najgorszej z rcnk i naszej - owce
  
  wykres_gestosc <- ggplot(przebieg, aes(przebieg$Liczba_ruchow,colour=przebieg$Strategia,fill=przebieg$Strategia))+
    geom_density(position="stack")+
    scale_color_manual(name="Srednia",values=c("#2166ac","#1b7837","#762a83"))+
    scale_fill_manual(values=c("#d1e5f0","#d9f0d3","#e7d4e8"))+
    geom_vline(data=srednia, aes(xintercept=srednia$grp.mean, color=srednia$Strategia), size=1.5)+
    geom_vline(data=mediana, aes(xintercept=as.numeric(mediana$grp.median), color=mediana$Strategia),size=1.5,linetype="dashed")+
    ylab("Liczba gier")+
    xlab("Liczba ruchow")+
    ggtitle(paste0("Porownanie gestosci dla strategii ",gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))," strategia_postMDiPR"," i strategia_anty_yolo"))+
    theme(panel.background = element_rect(fill="white"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20,angle = 0),
          title = element_text(size=25),
          legend.title = element_text(size=25),
          legend.text = element_text(size=20))+
    labs(fill="Strategia\n")
    
  
  
  
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
    labs(color="Zwierze\n",shape="Zwierze\n")#+
    #theme_tufte()
    
    
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
    labs(color="Zwierze\n",shape="Zwierze\n")#+
    #theme_tufte()
  
    
  
  #tytul
  tytul <- textGrob(paste0("Wizytowka\n SuperFarmer.SuperDziewczyn\n ",gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia))))),gp=gpar(fontsize=48, col="black", fontface = "bold"))
  
  #wykonanie
  autorzy <- textGrob("Aleksandra Dabrowska,\n Joanna Zbijewska",gp=gpar(fontsize=40, col="black"))
  
  #to co chcemy dolozyc jako tekst
  tekst <- textGrob((paste0("\nPrzedstawiamy ",gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))),"\n z pakietu SuperFarmer.SuperDziewczyn.\n Porownalysmy ja ze strategia strategia_postMDiPR \n z pakietu SuperFarmerMoc, dajaca najlepsze wyniki \n oraz strategia strategia_anty_yolo \n z pakietu SuperFarmerRCNK, ktora dawala najdluzsze gry.\n Porownanie przedstawilysmy na wykresie gestosci, \n na ktorym zaznaczone sa srednia i mediana dla kazdej strategii,\n a takze w tabeli z podstawowymi statystykami. \n Jednoczesnie dla przedstawionej strategii \n przedstawiamy zmiany liczby niektorych zwierzat w stadzie \n podczas pojedynczej gry.")),gp=gpar(fontsize=25, col="black"))
  
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
  
  rownames(statystyki) <- c(gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))),"strategia_postMDiPR","strategia_anty_yolo")
  colnames(statystyki)<-c("minimum","maksimum","rozrzut","odchylenie\n standardowe","srednia","srednia odcieta","mediana")

  
 
  
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1.8),bg_params = list(fill = c("#d1e5f0","#d9f0d3","#e7d4e8"))),
    colhead = list(fg_params=list(cex = 1.8)),
    rowhead = list(fg_params=list(cex = 2.0, fontface = "bold")))
  
  statystyki_tabela <- tableGrob(statystyki, theme = mytheme)
  
  #ustawienia na stronie 
  
  lay <- rbind(c(1,1,2,2,2,2),
               c(3,3,2,2,2,2),
               c(4,4,2,2,2,2),
               c(4,4,5,5,5,5),
               c(4,4,5,5,5,5),
               c(6,6,6,7,7,7),
               c(6,6,6,7,7,7))
  
  G <- arrangeGrob(grobs=list(tytul,wykres_gestosc,autorzy, tekst,statystyki_tabela,owce_i_kroliki,swinki_krowy_koniki),layout_matrix=lay) #na rownych skalach
  pdf(paste0(gsub("SuperFarmer.SuperDziewczyn::","",paste0(deparse(substitute(strategia)))),".pdf"), width = 29.7, height = 21) # Open a new pdf file
  grid.arrange(G)
  dev.off()
  
  
}