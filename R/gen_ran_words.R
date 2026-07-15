#' generate random words of specified string length
#'
#' This function generates a number of words with specified length.
#' When a prefix is given the generated part will be diminished to get a string of the requested length.
#' A seed can be specified for use of the RNG during generation of the words.
#' When this is done, the old RNG state will be saved and restore after exiting the function.
#'
#' @name gen_ran_words
#' @param nwords Integer specifying the number of words to generate
#' @param nchars Integer specifying the total number of characters in each word
#' @param prefix Character string to be used as prefix for each word. The prefix is contained in the number of characters
#' @param seed Integer to be used as random seed.
#' @return Character vector with the generated words
#' @export
#' @examples
#' \dontrun{
#' gen_ran_words(6,14,prefix='pic_',seed=13)
#' }


gen_ran_words <- function(nwords,
                          nchars,
                          prefix = NULL,
                          seed = NULL) {

  if (!is.null(seed))  {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
    } else {
      old_seed <- NULL
    }
    on.exit({
      if (!is.null(old_seed)) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    })
    set.seed(seed)
  }
  if (is.null(prefix)) prefix <- ""
  nc  <- nchars - nchar(prefix) - 1 # number of letnum characters to draw
  let <- letters
  num <- as.character(0:9)
  letnum <- c(let, num)
  c1 <- let[sample(seq_len(length(let)), size = nwords, replace = T)]
  c2 <- letnum[sample(seq_len(length(letnum)), size = nwords * nc, replace = T)]
  purrr::map_chr(seq_len(nwords), function(i) {
    paste0(c(prefix, c1[i], c2[(i - 1) * nc + (1:nc)]), collapse = "")
  })
}

