######################################################
#' @title ellipsetxt::countSentences
#' @description counts and returns the numbers of sentences in a text block
#' @param textblock : the text to analyse
#' @return sentence.count : the number of sentences in the block
#' @examples example
#'
#'
#' @export
count_sentences <- function(txt) {
  clean_corpus <- remove_titles(txt)

  df_sentences <- tibble::tibble(text =clean_corpus) %>% 
    tidytext::unnest_tokens(sentence, text, token="sentences",format="text")
  sentence_count <- nrow(df_sentences)
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
count_words <- function(txt) {
  txt <- gsub("[[:punct:][:blank:]]+", " ", txt)
  txt <- trimws(txt, which="both")

  count <- length(strsplit(txt, "\\s+")[[1]])

  return(count)
}

######################################################
#' @title ellipsetxt::countVecSentences
#' @description counts and returns the umber of sentences in a vector of text blocks (strings)
#' @param vector_of_corpus the vector of text strings
#' @return The number of sentences in the entire vector of corpus of text
#' @examples example
#' @importFrom "dplyr" "%>%"
#'
#'
#' @export
count_vector_sentences <- function(vector_of_corpus) {
  sentence.count <- 0

  for (i in 1:length(vecCorpus)) {
    clean_corpus <- removeTitles(vector_of_corpus[i])

    df_sentences <- tibble::tibble(text = clean_corpus) %>% 
      tidytext::unnest_tokens(sentence, text, token="sentences", format="text")
    sentence_count <- sentence_count + nrow(df_sentences)
  }
  return(sentence_count)
}