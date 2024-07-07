######################################################
#' @title clessnverse::evaluateRelevanceIndex
#' @description Evaluates and returns the relevance index of a text block (string)
#' @param txt - A character string
#' @param dictionary  - A vector of strings containing regex or words or words combinations
#'			               or an objet of type dictionary from the quanteda package
#' @param base	      - A string containing either "sentence" or "paragraph" which tells the
#'			                the function on which base to compute the index.
#'			                If base = "sentence" then every sentence containing one or more terms
#'			                matching in the dictionary will score 1 in the calculation.
#'			                At the end of the count the sum is devided by the number of sentences.
#'			                If base = "paragraph" the same logic applies but at the paragraph level
#'			                instead of the sentence level
#' @param method      - A string describing the method used for matching terms.
#'			                If method = "wordsmatch" or "regex" then dictionary must be a vector of
#'			                strings containing words, words sequences or regex.
#'			                If method = "dfm" then dictionary must be of type quanteda dictionary
#' @return a double objet representing the relevance index of a subjet in the text provided
#' @examples example
#'
#'
#'
#' @export
evaluate_relevance_index <- function(txt, dictionary, base = "sentence", method = "dfm") {
  relevance_index <- 0

  if (is.na(txt) || is.null(txt) || nchar(txt) == 0) {
    return(relevance_index)
  }

  if (base == "paragraph") {
    vec_txt <- vector()
    vec_txt <- strsplit(txt, "\n\n")[[1]]

    for (i in 1:length(vec_txt)) {
      if (method == "dfm") {
        ###  DFM METHOD
        string_to_check <- vec_txt[i]
        if (!is.na(string_to_check)) {
          # repérer le nombre de mots du dictionnaire
          # sur le sujet d'intérêt présents dans le paragraphe
          dfm_a <- quanteda::dfm(string_to_check, dictionary = dictionary)
          # compter les phrases mentionnant le
          # sujet d'intérêt en excluant les NA
          if (length(dfm_a@x) != 0 && dfm_a@x > 0) relevance_index <- relevance_index + 1
        }
      }
      else {
        ### REGEX OU WORDS MATCHING
        string_to_check <- stringr::str_replace_all(vec_txt[i], "[[:punct:]]", "")
        if ( TRUE %in% stringr::str_detect(string_to_check, dictionary) ) relevance_index <- relevance_index + 1
      }
    }

    relevance_index <- relevance_index / length(vec_txt)
  } #if base == paragraph

  if (base == "sentence") {
    # M., Mr. et Dr. ne signifient pas une fin de phrase, donc supprimer ces mots
    txt <- remove_titles(txt)

    # séparer le texte en phrases, tout en minuscules
    df_sentences <- tibble::tibble(text = txt) %>%
      tidytext::unnest_tokens(sentence, text, token="sentences",format="text", to_lower = T)

    count <- 0

    for (i in 1:nrow(df_sentences)) {
      string_to_check <- df_sentences$sentence[i]
      if (method == "dfm") {
        ###  DFM METHOD
        if (!is.na(string_to_check)) {
          # repérer le nombre de mots du dictionnaire
          # sur le sujet d'intérêt présents dans la phrase
          dfm_a <- quanteda::dfm(string_to_check, dictionary = dictionary)
          # compter les phrases mentionnant le
          # sujet d'intérêt en excluant les NA
          if (length(dfm_a@x) != 0 && dfm_a@x > 0) count <- count + 1
        }
      } else {
        ### REGEX OU WORDS MATCHING
        if ( TRUE %in% stringr::str_detect(stringr::str_replace_all(string_to_check, "[[:punct:]]", ""), dictionary) )
          count <- count + 1
      }
    }
    # pondérer les phrases concernant la COVID-19 en fonction
    # du nombre total de phrases dans l'intervention
    relevance_index <- count / nrow(df_sentences)
    if (is.nan(relevance_index)) relevance_index <- 0
  } #if base == sentence

  return(relevance_index)
}

