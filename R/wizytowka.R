#'Wizytowka do strategii owce
#'
#'
#'
#'
#'
#'


wizytowka <- function(strategia){
  #dane do wizytówki
  library(plyr)
  library(ggplot2)
  przebieg_gry <- SuperFarmer.SuperDziewczyn::gra(strategia)
  macierz_przebiegu_gry <- przebieg_gry[[1]]
  
  
  przebieg_100_gier_superdziewczyn <- SuperFarmer.SuperDziewczyn::badaj_gre(strategia,powtorzenia = 100)
  przebieg_100_gier_moc <- SuperFarmerMoc::badaj_gre(SuperFarmerMoc::strategia_postMDiPR,liczba_prob=100)
  przebieg_100_gier_da <- SuperFarmer.SuperDziewczyn::badaj_gre(SuperFarmerDA::strategia_DKA,powtorzenia=100)
  
  #jako ramki danych
  macierz_przebiegu_gry<-cbind(macierz_przebiegu_gry,c(1:przebieg_gry[[2]]))
  colnames(macierz_przebiegu_gry)[8] <- "Numer_kolejki"
  przebieg_gry <- as.data.frame(macierz_przebiegu_gry)
  przebieg_100_gier_superdziewczyn <- as.data.frame(przebieg_100_gier_superdziewczyn)
  przebieg_100_gier_moc <- as.data.frame(przebieg_100_gier_moc)
  przebieg_100_gier_da <- as.data.frame(przebieg_100_gier_da)
  
  colnames(przebieg_100_gier_superdziewczyn) <- "Liczba_ruchow"
  colnames(przebieg_100_gier_moc) <- "Liczba_ruchow"
  colnames(przebieg_100_gier_da) <- "Liczba_ruchow"
  
  przebieg_100_gier_superdziewczyn <-cbind(przebieg_100_gier_superdziewczyn, sample(deparse(substitute(strategia)),100, replace=TRUE))
  
  przebieg_100_gier_moc <-cbind(przebieg_100_gier_moc, sample("SuperFarmerMoc::strategia_postMDiPR",100, replace=TRUE))
  
  przebieg_100_gier_da <-cbind(przebieg_100_gier_da, sample("SuperFarmerDA::strategia_DKA",100, replace=TRUE))
  
  colnames(przebieg_100_gier_superdziewczyn)[2] <- "Strategia"
  colnames(przebieg_100_gier_moc)[2] <- "Strategia"
  colnames(przebieg_100_gier_da)[2] <- "Strategia"
  
  przebieg <- rbind(przebieg_100_gier_superdziewczyn,przebieg_100_gier_moc)
  przebieg <- rbind(przebieg,przebieg_100_gier_da)
  
  srednia <-plyr::ddply(przebieg, "Strategia", summarise, grp.mean=mean(Liczba_ruchow))
  
  mediana <- plyr::ddply(przebieg,"Strategia",summarise,grp.median=median(Liczba_ruchow))
  #wykres gestosci dla najlepszej strategii z pakietu moc, najgorszej z da i naszej - owce
  #narazie nazwa pakiet ale powinnnismy to opisac od strategii, ale jeszcze nie wiem jak
  wykres_gestosc <- ggplot2::ggplot(przebieg, aes(Liczba_ruchow,..count..,colour=Strategia,fill=Strategia))+
    geom_density(position="stack")+
    scale_color_manual(values=c("#2166ac","#1b7837","#762a83"))+
    scale_fill_manual(values=c("#d1e5f0","#d9f0d3","#e7d4e8"))+
    geom_vline(data=srednia, aes(xintercept=grp.mean, color=Strategia),linetype="dashed")+
    geom_vline(data=mediana, aes(xintercept=grp.median, color=Strategia))+
    ylab("Liczba gier")+
    xlab("Liczba ruchów")+
    ggtitle(paste0("Porownanie gestosci dla strategii ",deparse(substitute(strategia)),",\nSuperFarmerMoc::strategia_postMDiPR i SuperFarmerDA::strategia_DKA"))
  
  
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
  
  owce_i_kroliki <- ggplot(zwierzatka,aes(Numer_kolejki,liczba,col=zwierze,shape=zwierze))+
    geom_point(size=3)+
    ylab("Liczba zwierzatek")+
    xlab("Numer kolejki")+
    ggtitle("Liczba królików i owiec w pojedynczej grze")
    
    
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

  swinki_krowy_koniki <- ggplot(zwierzatka_duze,aes(Numer_kolejki,liczba,col=zwierze,shape=zwierze))+
    geom_point(size=3)+
    ylab("Liczba zwierzatek")+
    xlab("Numer kolejki")+
    ggtitle("Liczba swinek, krów i koników w pojedynczej grze")
  
    
  
  #tytul
  tytul <- grid::textGrob("Wizytówka strategii owce Aleksandra Dabrowska, Joanna Zbijewska")
  
  
  #to co chcemy dolozyc jako tekst
  tekst <- grid::textGrob("Strategia owce czy cos")
  
  #statystyki na wczesniej przygotowanych danych
  
  minimum <- c(min(przebieg_100_gier_superdziewczyn$Liczba_ruchow),min(przebieg_100_gier_moc$Liczba_ruchow),min(przebieg_100_gier_da$Liczba_ruchow))
  
  maksimum <- c(max(przebieg_100_gier_superdziewczyn$Liczba_ruchow),max(przebieg_100_gier_moc$Liczba_ruchow),max(przebieg_100_gier_da$Liczba_ruchow))
  
  odchylenie <- c(sd(przebieg_100_gier_superdziewczyn$Liczba_ruchow),sd(przebieg_100_gier_moc$Liczba_ruchow),sd(przebieg_100_gier_da$Liczba_ruchow))
  odchylenie <- round(odchylenie, digits = 2)
  
  srednia_zwykla <- c(mean(przebieg_100_gier_superdziewczyn$Liczba_ruchow),mean(przebieg_100_gier_moc$Liczba_ruchow),mean(przebieg_100_gier_da$Liczba_ruchow))
  
  srednia_odcieta <- c(mean(przebieg_100_gier_superdziewczyn$Liczba_ruchow,trim=0.2),mean(przebieg_100_gier_moc$Liczba_ruchow,trim=0.2),mean(przebieg_100_gier_da$Liczba_ruchow,trim=0.2))
  
  srednia_odcieta <- round(srednia_odcieta, digits = 2)
  
  
  mediana_zwykla <- c(median(przebieg_100_gier_superdziewczyn$Liczba_ruchow),median(przebieg_100_gier_moc$Liczba_ruchow),median(przebieg_100_gier_da$Liczba_ruchow))
  
  rozrzut <- maksimum-minimum
  
  statystyki <- rbind(minimum,maksimum)
  statystyki <- rbind(statystyki,rozrzut)
  statystyki <- rbind(statystyki,odchylenie)
  statystyki <- rbind(statystyki,srednia_zwykla)
  statystyki <- rbind(statystyki,srednia_odcieta)
  statystyki <- rbind(statystyki,mediana_zwykla)
 
  statystyki <- as.data.frame(statystyki)
  
  colnames(statystyki) <- c("superdziewczyn","moc","da")
  
  statystyki <- gridExtra::tableGrob(statystyki)
  
  
  
  
  #ustawienia na stronie 
  
  layout <- rbind(c(1,1,1,1,2,2,2,2),
                  c(3,3,3,3,2,2,2,2),
                  c(3,3,3,3,2,2,2,2),
                  c(3,3,3,3,4,4,4,4),
                  c(5,5,5,5,6,6,6,6),
                  c(5,5,5,5,6,6,6,6))
  
  
  
  #oba <-gridExtra::grid.arrange(grobs=c(tytul,wykres_gestosc, tekst,statystyki,owce_i_kroliki,swinki_krowy_koniki),layout_matrix=layout) #na rownych skalach
  
  dd <- gridExtra::grid.arrange(tytul,tekst,wykres_gestosc,statystyki,owce_i_kroliki,swinki_krowy_koniki,nrow=3,ncol=2,widths=c(1.2,1.2),heights=c(1, 3, 3))
  
  
  ggplot2::ggsave("strategia_owce.pdf",dd)
  
}