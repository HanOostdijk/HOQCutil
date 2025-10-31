#' translate Dutch month names into English
#'
#' @name maanden_NL2EN
#' @param cv Character vector with elements that can contain Dutch month indications
#' @return Character vector with the same length as `cv` with Dutch months translated into English
#' @export
#' @examples
#'
#' maanden_NL2EN(c("03 januari 2013", "5 mei 2025"))
#'
#'

maanden_NL2EN <- function(cv) {
  stringr::str_replace_all(tolower(cv), maanden)
}

maanden <- c(
  "januari" = "Jan", "februari" = "Feb", "maart"= "Mar",
  "april" = "Apr", "mei" = "May", "juni"= "Jun",
  "juli" = "July", "augustus" = "Aug", "september"= "Sep",
  "oktober" = "Oct", "november" = "Nov", "december"= "Dec",
  "jan" = "Jan", "feb" = "Feb", "mrt"= "Mar",
  "apr" = "Apr", "mei" = "May", "jun"= "Jun",
  "jul" = "July", "aug" = "Aug", "sep"= "Sep",
  "okt" = "Oct", "nov" = "Nov", "dec"= "Dec"
)


