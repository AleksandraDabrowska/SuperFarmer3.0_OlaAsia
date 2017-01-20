#'Wykres obrazujacy przebieg pojedynczej gry
#'
#'@param tabela tabela danych do wykresu
#'@param tytul tytul wykresu
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_bar
#' 
#'@return slupki wykres slupkowy
#'
#'
#'@export

wykres_wszystkie_zwierzatka <- function(tabela,tytul){
  slupki <- ggplot(tabela, aes(Numer_kolejki,Liczba, fill=Zwierze))+
    scale_fill_manual(values=c("brown","black","green","purple","yellow","#FFCCCC","#FF33CC"))+
    geom_bar(stat="identity")+
    ylab("Liczba zwierzatek")+
    ylim(c(0,30))+
    xlab("Numer kolejki")+
    ggtitle(tytul)+
    theme(panel.background = element_rect(fill="white"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20,angle = 0),
          title = element_text(size=25),
          legend.text = element_text(size=20),
          legend.title = element_text(size=25))+
    labs(color="Zwierze\n")
  return(slupki)
  
  
}