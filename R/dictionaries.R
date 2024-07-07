#' Calculate dictionary expression mentions in a text.
#'
#' This function creates a data.frame which includes one column
#' with each observation's ID as well as one column for each
#' category of the inputted dictionary.
#'
#' @param data An object of type data.frame.
#' @param text The name of the character variable for which
#' dictionary expressions are to be matched against.
#' @param dictionary An object of type dictionary.
#' @param verbose TRUE or FALSE Whether activity messages are displayed on
#' the screen.
#' @return A data.frame which includes one more column than
#' the number of dictionary categories. The first column is named
#' doc_id. Each other column is named after a dictionary category.
#' @export
#' @import dplyr
#' @importFrom crayon yellow
#' @importFrom crayon green
#' @import quanteda
#' @importFrom tictoc tic
#' @importFrom tictoc toc
#' @author CLESSN
#' @examples
#'
#' \dontrun{
#'
#' # Calculate the number of dictionary expression mentions in
#' a list of attitudes.
#'
#' run_dictionary(data.frame(colnames(attitude)),
#' text = colnames(attitude),
#' dictionary = quanteda::data_dictionary_LSD2015)
#' }
run_dictionary <- function(data, text, dictionary, verbose=TRUE) {
  if (verbose) tictoc::tic() # calculate number of seconds for function execution
  if (is.data.frame(data) != "TRUE") {
    stop(crayon::yellow('the argument "data" needs to be a dataframe'))
  }
  data <- data %>% dplyr::mutate(text = {
    {
      text
    }
  })
  if (is.character(data$text) != "TRUE") {
    stop(crayon::yellow('The variable "text" needs to be a character vector'))
  }
  corpus <- quanteda::tokens(data$text) # transforms the vector into
  # tokens for dictionary analysis
  if (quanteda::is.dictionary(dictionary) != "TRUE") {
    stop(crayon::yellow(
      paste0(
        'Your "dictionary" needs to be in a dictionary format\n',
        ' For more information:"',
        ' https://quanteda.io/reference/dictionary.html'
      )
    ))
  }
  dfm <- # applies the dictionary to the variable corpus and creates a
    # dfm (document-feature matrix). Applies tolower() by default
    quanteda::dfm(quanteda::tokens_lookup(corpus, dictionary, nested_scope = "dictionary"))
  if (verbose) message(crayon::green("100% expressions/words found"))
  dataFinal <- quanteda::convert(dfm, to = "data.frame")
  if (verbose) tictoc::toc()
  return(dataFinal)
}