#'Wizualizacja n gier
#'
#'Funkcja `wizualizacja_gry()` jako argumenty pobiera wektor dlugosci n zawierajacy informacje o dlugosci poszczegolnych gier oraz typy wykresow ktore chcemy uzyskac (mozemy wybierac sposrod : histogram, pudelkowy, skrzypcowy, pudelkowy + skrzypcowy, skrzypcowy + pudelkowy).
#'
#'@param wektor wektor zawierajacy dane dotyczace dlugosci trwania poszczegolnych gier
#'@param typ_wykresu1 wykres jaki chcemy uzyskac
#'@param typ_wykresu2 opcjonalnie druga warstwa na wykresie
#'
#'@return wykres
#'
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 geom_histogram
#'@importFrom ggplot2 labs
#'@importFrom ggplot2 theme
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 geom_density
#'@importFrom ggplot2 geom_boxplot
#'@importFrom ggplot2 xlab
#'@importFrom ggplot2 ylab
#'@importFrom ggplot2 element_blank
#'@importFrom ggplot2 geom_violin
#'
#'@export

wizualizacja_gry <- function(wektor, typ_wykresu1 = "histogram", typ_wykresu2=NULL){
  czasy <- as.data.frame(wektor)
  colnames(czasy) <- "Czas_gry"
  if(typ_wykresu1 == "histogram"&& is.null(typ_wykresu2)==TRUE){
      ggplot(data=czasy, aes(czasy$Czas_gry))+
        geom_histogram(binwidth=1,col="#0033FF", fill="#33CCCC") +
        labs(title="Rozklad czasu gry") +
        labs(x="Czas gry", y="Liczba gier")
    }else{
      if(typ_wykresu1 == "pudelkowy" && is.null(typ_wykresu2)==TRUE){
        ggplot(data=czasy,aes(x=factor(0),czasy$Czas_gry))+
          geom_boxplot(fill="#0033FF")+
          theme(axis.ticks.x=element_blank())+
          ylab("Czas trwania gier") +
          xlab("Badaj gre")+
          labs(title="Rozklad czasu gry")
      }else{
        if(typ_wykresu1 == "skrzypcowy" && is.null(typ_wykresu2)==TRUE){
          ggplot(data=czasy,aes(x=factor(0),czasy$Czas_gry))+
            geom_violin(fill="#33CCCC")+
            theme(axis.ticks.x=element_blank())+
            ylab("Czas trwania gier") +
            xlab("Badaj gre")+
            labs(title="Rozklad czasu gry")
        }else{
          if((typ_wykresu1=="pudelkowy"&&typ_wykresu2=="skrzypcowy")||(typ_wykresu1=="skrzypcowy"&&typ_wykresu2=="pudelkowy")){
            ggplot(data=czasy,aes(x=factor(0),czasy$Czas_gry))+
            geom_violin(fill="#33CCCC")+
            geom_boxplot(fill="#0033FF", outlier.alpha = 0, alpha = 0.5)+
            theme(axis.ticks.x=element_blank())+
            ylab("Czas trwania gier") +
            xlab("Badaj gre")+
            labs(title="Rozklad czasu gry")
        }else{
        ("Podaj poprawny typ wykresu")
        }}}}}
