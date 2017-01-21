# Trzecia faza projektu zaliczeniowego na przedmiot Programowanie w R i Wizualizacje Danych na MIMUW 2016/2017
###Tu powstaje wizytówka opisująca strategie w pakiecie SuperFarmer.SuperDziewczyn

####Twórcy:
####Aleksandra Dąbrowska
####Joanna Zbijewska 

#O pakiecie SuperFarmer.SuperDziewczyn

######Instalacja pakietu:
```{r}
library(devtools)
install_github("AleksandraDabrowska/SuperFarmer3.0_OlaAsia")
```

######Pojedyncza rozgrywka:
```{r}
gra(strategia)
```
Opcjonalnie do funkcji `gra` można dobrać odpowiednie parametry np. `warunek_wygranej`, `co_zostawia_lis` itp.

######Powtórzenia gry:
```{r}
badaj_gre(strategia, powtorzenia)
```

######Strategie:
Strategie zaimplementowane w pakiecie : `strategia_owce`, `strategia_wymian_0_0_0_0` i `strategia_wymian_1_1_1_1`.


######Wizytówki strategii w pakiecie
Wizytówki z pakietu SuperFarmer.SuperDziewczyn pozwalają na porównanie zaimplementowanych w tym pakiecie strategii ze strategią wybraną jako najszybszą i jako najwolniejszą z przedstawionych na zajęciach.
Wynikiem działania tej funkcji jest plik pdf (w folderze `inst`) zawierający wykresy porównujące wybraną strategie, statystyki, wartość decyli dla trzech strategii oraz wizualizację pojedynczej gry polegającą na przdstawieniu liczebności stada oraz jego składu, w zależności od numeru ruchu

#####Podziękowania.

Bardzo dziękujemy Alicji Gosiewskiej i Hannie Kranas za współpracę w poprzednich fazach tego projektu. <3


