######################################################
#' @title ellipsetxt::countSentences
#' @description counts and returns the numbers of sentences in a text block
#' @param textblock : the text to analyse
#' @return sentence.count : the number of sentences in the block
#' @examples example
#'
#'
#' @export
countSentences <- function(txt) {
  cleanCorpus <- stringr::str_replace_all(string = txt, pattern = "M\\.", replacement = "")
  cleanCorpus <- stringr::str_replace_all(string = cleanCorpus, pattern = "Mr\\.", replacement = "")
  cleanCorpus <- stringr::str_replace_all(string = cleanCorpus, pattern = "Dr\\.", replacement = "")

  dfSentences <- tibble::tibble(text = cleanCorpus) %>% tidytext::unnest_tokens(sentence, text, token="sentences",
                                                              format="text")
  sentence.count <- nrow(dfSentences)
  return(sentence.count)
}

######################################################
#' @title ellipsetxt::wordCount
#' @description returns the number of words in a string
#' @param txt the string
#' @return an integer
#' @examples example
#'
#'
#'
#' @export
countWords <- function(txt) {
  txt <- gsub("[[:punct:][:blank:]]+", " ", txt)
  txt <- trimws(txt, which="both")

  count <- length(strsplit(txt, "\\s+")[[1]])

  return(count)
}

######################################################
#' @title ellipsetxt::countVecSentences
#' @description counts and returns the umber of sentences in a vector of text blocks (strings)
#' @param vecCorpus the vector of text strings
#' @return
#' @examples example
#' @importFrom "dplyr" "%>%"
#'
#'
#' @export
countVecSentences <- function(vecCorpus) {
  sentence.count <- 0

  for (i in 1:length(vecCorpus)) {
    cleanCorpus <- stringr::str_replace_all(string = vecCorpus[i], pattern = "M\\.", replacement = "")
    cleanCorpus <- stringr::str_replace_all(string = cleanCorpus, pattern = "Mr\\.", replacement = "")
    cleanCorpus <- stringr::str_replace_all(string = cleanCorpus, pattern = "Dr\\.", replacement = "")
    
    dfSentences <- tibble::tibble(text = cleanCorpus) %>% tidytext::unnest_tokens(sentence, text, token="sentences",
                                                                format="text")
    sentence.count <- sentence.count + nrow(dfSentences)# - sum(words(vecCorpus[i]) %in% patterns.titres == TRUE)
  }
  return(sentence.count)
}