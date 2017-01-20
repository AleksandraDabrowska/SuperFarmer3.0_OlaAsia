#'Wykres obrazujacy przebieg pojedynczej gry
#'
#'Funkcja wykres_wszytskie_zwierzatka(), rysuje wykres slupkowy zawierajacy liczby poszczegolnych zwierzat dla danej kolejki, dla jednej, losowej gry.
#'
#'@param tabela Ramka danych zwierajaca liczebnosc zwiezat w rozgrywce. Musi zawierac kolumny Numer_kolejki, liczba, zwierze.
#'@param tytul Argument o klasie string definiujacy tytul wykresu.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_fill_brewer
#' 
#'@return slupki wykres slupkowy
#'
#'@examples
#'\dontrun{
#'wykres <- wykres_wszytskie_zwierzatka(tabela_gra_owce,tytul_tabeli)
#'}
#'@seealso wykres_zwierzat
#'@export

wykres_wszystkie_zwierzatka <- function(tabela,tytul){
  slupki <- ggplot(tabela, aes(tabela$Numer_kolejki,tabela$Liczba, fill=tabela$Zwierze))+
    scale_fill_brewer(name="Zwierze",palette="Set1")+
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