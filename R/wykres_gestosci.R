#' Generowanie wykresu gestosci dla trzech strategii
#'
#' Funkcja wykres_gestosci() sluzy do tworzenia wykresu gestosci liczby ruchow dla trzech strategii, a takze nalozenia na niego
#' pionowych linii z med i srednia dla liczby ruchow dla kazdej strategii.
#'
#' @param dane Jest ramka danych zawierjaca trzy grupy dnaych do rysowania gestosci zawierajace nazwy strategii w kolumnie Strategia oraz liczby ruchow w pojedynczej grze w kolumnie Liczba_ruchow.
#' @param strat Nazwa strategii, ktora na wykresie porownujemy.
#' @param med Ramka danych z mediana liczby ruchow dla trzech strategii.
#' @param sr Ramka danych ze srednia liczby ruchow dla trzech strategii.
#'
#' @return Zwraca wykres gestosci.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_text
#'
#' @examples
#'\dontrun{
#'gestosc <- wykres_gestosci(tabela_wykres,SuperFarmer.SuperDziewczyn::strategia_owce,mediany,srednie)
#'}
#'
#' @export

wykres_gestosci <- function(dane, strat, med, sr){
  wykres <- ggplot(dane, aes(dane$Liczba_ruchow,colour=dane$Strategia,fill=dane$Strategia))+
    geom_density(size=2, alpha=0.4)+
    scale_color_manual(name="Srednia\n",values=c("#2166ac","#1b7837","#762a83"))+
    scale_fill_manual(values=c("#d1e5f0","#d9f0d3","#e7d4e8"))+
    geom_vline(data=sr, aes(xintercept=sr$grp.mean, color=sr$Strategia), size=1.5)+
    geom_vline(data=med, aes(xintercept=as.numeric(med$grp.median), color=med$Strategia),size=1.5,linetype="dashed")+
    ylab("Liczba gier")+
    xlab("Liczba ruchów")+
    ggtitle(paste0("Porównanie gęstości dla strategii: ",strat,",\nstrategia_postMDiPR"," i strategia_anty_yolo"))+
    theme(panel.background = element_rect(fill="white"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20,angle = 0),
          title = element_text(size=25),
          legend.position="none")+
    labs(fill="Strategia\n")
  return(wykres)
}
