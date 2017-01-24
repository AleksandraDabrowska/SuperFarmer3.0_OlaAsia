#' Zapisanie obiektu do PDF.
#'
#' Funkcja zapisz() zapisuje zadany obiekt do pliku PDF w folderze inst.
#'
#' @param co Definiowanie obiektu, ktory chce sie zapisac.
#' @param plik Nazwa pliku docelowego bez rozszerzenia.
#'
#' @importFrom grDevices dev.off
#' @importFrom Cairo CairoPDF
#'
#' @export

zapisz <- function(co, plik){
  CairoPDF(paste0(plik,".pdf"), width = 29.7, height = 21)
  co
  dev.off()
}
