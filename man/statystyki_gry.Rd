% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statystyki_gry.R
\name{statystyki_gry}
\alias{statystyki_gry}
\title{Funkcja obliczajaca podstawowe statystki dla wybranej wczesniej liczby gier}
\usage{
statystyki_gry(wektor)
}
\arguments{
\item{wektor}{wektor zawierajacy dane dotyczace dlugosci trwania poszczegolnych gier}
}
\value{
statystyki dla gry
}
\description{
Funkcja`statystyki_gry()` przyjmuje wektor dlugosci n zawierajacy informacje o dlugosci poszczegolnych gier. Zwraca podstawowe statystyki (min,max,kwantyle,mediana,srednia) dotyczace dlugosci trwania gier.
}
\examples{
wektor <- c(20,40,20,10,12,55)
statystyki_gry(wektor)

}

