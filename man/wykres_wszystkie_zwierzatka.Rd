% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wykres_wszystkie_zwierzatka.R
\name{wykres_wszystkie_zwierzatka}
\alias{wykres_wszystkie_zwierzatka}
\title{Wykres obrazujacy przebieg pojedynczej gry}
\usage{
wykres_wszystkie_zwierzatka(tabela, tytul)
}
\arguments{
\item{tabela}{Ramka danych zwierajaca liczebnosc zwiezat w rozgrywce. Musi zawierac kolumny Numer_kolejki, liczba, zwierze.}

\item{tytul}{Argument o klasie string definiujacy tytul wykresu.}
}
\value{
slupki wykres slupkowy
}
\description{
Funkcja wykres_wszytskie_zwierzatka(), rysuje wykres slupkowy zawierajacy liczby poszczegolnych zwierzat dla danej kolejki, dla jednej, losowej gry.
}
\examples{
\dontrun{
wykres <- wykres_wszytskie_zwierzatka(tabela_gra_owce,tytul_tabeli)
}
}
\seealso{
wykres_zwierzat
}

