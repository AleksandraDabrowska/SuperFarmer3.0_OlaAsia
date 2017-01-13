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
  library(emojifont)
  library(ggplot2)
  przebieg_gry <- SuperFarmer.SuperDziewczyn::gra(strategia_owce)
  macierz_przebiegu_gry <- przebieg_gry[[1]]
  przebieg_100_gier_superdziewczyn <- SuperFarmer.SuperDziewczyn::badaj_gre(strategia_owce,powtorzenia = 100)
  przebieg_100_gier_moc <- SuperFarmerMoc::badaj_gre(SuperFarmerMoc::strategia_postMDiPR,liczba_prob=100)
  przebieg_100_gier_da <- SuperFarmer.SuperDziewczyn::badaj_gre(SuperFarmerDA::strategia_DKA,powtorzenia=100)
  
  #jako ramki danych
  macierz_przebiegu_gry<-cbind(macierz_przebiegu_gry,c(1:przebieg_gry[[2]]))
  colnames(macierz_przebiegu_gry)[8] <- "numer_kolejki"
  przebieg_gry <- as.data.frame(macierz_przebiegu_gry)
  przebieg_100_gier_superdziewczyn <- as.data.frame(przebieg_100_gier_superdziewczyn)
  przebieg_100_gier_moc <- as.data.frame(przebieg_100_gier_moc)
  przebieg_100_gier_da <- as.data.frame(przebieg_100_gier_da)
  
  colnames(przebieg_100_gier_superdziewczyn) <- "Liczba_ruchow"
  colnames(przebieg_100_gier_moc) <- "Liczba_ruchow"
  colnames(przebieg_100_gier_da) <- "Liczba_ruchow"
  
  przebieg_100_gier_superdziewczyn <-cbind(przebieg_100_gier_superdziewczyn, sample("superdziewczyn",100, replace=TRUE))
  colnames
  
  przebieg_100_gier_moc <-cbind(przebieg_100_gier_moc, sample("moc",100, replace=TRUE))
  
  przebieg_100_gier_da <-cbind(przebieg_100_gier_da, sample("da",100, replace=TRUE))
  
  colnames(przebieg_100_gier_superdziewczyn)[2] <- "Pakiet"
  colnames(przebieg_100_gier_moc)[2] <- "Pakiet"
  colnames(przebieg_100_gier_da)[2] <- "Pakiet"
  
  przebieg <- rbind(przebieg_100_gier_superdziewczyn,przebieg_100_gier_moc)
  przebieg <- rbind(przebieg,przebieg_100_gier_da)
  srednia <-ddply(przebieg, "Pakiet", summarise, grp.mean=mean(Liczba_ruchow))
  #wykres gestosci dla najlepszej strategii z pakietu moc, najgorszej z da i naszej - owce
  #narazie nazwa pakiet ale powinnnismy to opisac od strategii, ale jeszcze nie wiem jak
  wykres_gestosc <- ggplot(przebieg, aes(Liczba_ruchow,..count..,colour=Pakiet))+geom_density(position="stack")+scale_color_manual(values=c("blue","green","red"))+geom_vline(data=srednia, aes(xintercept=grp.mean, color=Pakiet),linetype="dashed")
  
  
  #tu mam dolozyc emoji, trzeba pobrac emoji z githuba, ale nadal nie dziala

  load.emojifont('OpenSansEmoji.ttf')
  owce <- przebieg_gry[,c(2,8)]
  owce <- data.frame(owce,zwierzeta=sample(emoji("sheep"),nrow(owce),replace=TRUE))
  przebieg_gry_owce <- ggplot(przebieg_gry,(aes(numer_kolejki,owca, label=zwierzeta)))+geom_point(size=0)+ geom_text(family="OpenSansEmoji", size=6)
 
  przebieg_100_gier <- SuperFarmer.SuperDziewczyn::wizualizacja_gry(przebieg_100_gier_superdziewczyn, "histogram")
  
  
  #to co chcemy dolozyc jako tekst
  tekst <- grid::textGrob("Ola i Asia")
  oba <-gridExtra::grid.arrange(tekst,przebieg_gry_owce,wykres_gestosc,ncol=2) #na rownych skalach
  
  
  ggplot2::ggsave("strategia_owce.pdf",oba)
  
}