######################################################
#' @title ellipsetxt::cleanCorpus
#' @description counts and returns the umber of sentences in a vector of text blocks (strings)
#' @param txt the vector of text strings
#' @return a cleaned version of txt without M., Mr. and Dr.
#' @examples example
#' @importFrom "dplyr" "%>%"
#'
#'
#' @export
remove_titles <- function(txt) {
  clean_corpus <- stringr::str_replace_all(string = txt, pattern = "M\\.", replacement = "")
  clean_corpus <- stringr::str_replace_all(string = clean_corpus, pattern = "Mr\\.", replacement = "")
  clean_corpus <- stringr::str_replace_all(string = clean_corpus, pattern = "Dr\\.", replacement = "")
  return(clean_corpus)
}