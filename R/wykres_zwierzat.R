#' Generowanie wykresu dla liczebnosci zwierzat podczas rozgrywki.
#' 
#' Funkcja wykres_zwierzat sluzy do stworzenia wykresu punktowego, w ktorym punkty odpowiadaja liczbie zwierzat w pojedynczej
#' rozgrywce w SuperFarmera.
#' 
#' @param dane Ramka danych zwierajaca liczebnosc zwiezat w rozgrywce. Musi zawierac kolumny Numer_kolejki, liczba, zwierze.
#' @param tytul Argument o klasie string definiujacy tytul wykresu.
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggtitle
#' 
#' @export


wykres_zwierzat <- function(dane,tytul){
  zwierzaki <- ggplot(dane,aes(dane$Numer_kolejki,dane$liczba,col=dane$zwierze,shape=dane$zwierze))+
    geom_point(size=3)+
    ylab("Liczba zwierzatek")+
    xlab("Numer kolejki")+
    ggtitle(tytul)+
    theme(panel.background = element_rect(fill="white"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20,angle = 0),
          title = element_text(size=30),
          legend.text = element_text(size=20),
          legend.title = element_text(size=25))+
    labs(color="Zwierze\n",shape="Zwierze\n")
  return(zwierzaki)
}