#' categorises transactions in a data.frame
#'
#' Based on categorisation rule a category field is inserted in a data.frame See **Details**
#'
#' @name categorise_transactions
#' @param .data Data.frame that contains transactions
#' @param .column Character string that contains data used for the categorisation
#' @param .rescolumn Name of the (new) column that will contain the resulting category
#' @param .default Character string to use as default category
#' @param ... One or more named character vectors that contain the classification. See **Details**
#' @return Data.frame that is `.data` with the (replaced or) new column `.rescolumn` with the category
#' @section Details:
#' The category is determined by creating a `dplyr::case_when` statement based on the named character vectors
#' in `...` . \cr
#' This means that the order of the vectors could be important. \cr
#' An example shows how a named character vector is translated to a formula for the `dplyr::case_when` function. \cr
#' When one of the named characters is `rentes = c("rente", "interest")` the following formula is generated : \cr
#' `(stringr::str_detect(.column, stringr::fixed('rente', ignore_case = TRUE))) | ` \cr
#' `  (stringr::str_detect(.column, stringr::fixed('interest', ignore_case = TRUE))) ~ 'rentes'` \cr
#' @export
#' @examples
#' \dontrun{
#' df1 <- data.frame(Omschrijving = c("Rente betaling", "Huur", "Interest", "Onbekend"))
#' df1 |> categorise_transactions(
#'   Omschrijving,
#'   .default="weet niet",
#'   .rescolumn="cat",
#'   # categorisation rules follow:
#'   rentes = c("rente", "interest"),
#'   huren = c("huur"))
#' }
#'

categorise_transactions <- function(.data, .column, .rescolumn="soort", .default="onbekend", ...) {
  col_sym <- rlang::ensym(.column)
  mapping <- rlang::list2(...)

  # Bouw de lijst met formules
  formulas <- purrr::imap(mapping, function(zoektermen, label) {

    # Maak de individuele str_detect expressies
    detect_calls <- purrr::map(zoektermen, function(term) {
      rlang::expr(stringr::str_detect(!!col_sym, stringr::fixed(!!term, ignore_case = TRUE)))
    })

    # Verbind ze met |
    lhs <- purrr::reduce(detect_calls, function(left, right) rlang::expr(!!left | !!right))

    # Maak de formule ZONDER een specifieke environment vast te leggen
    # We gebruiken hier de '~' operator direct in een expressie
    rlang::expr(!!lhs ~ !!label)
  })

  # Gebruik inject() om de !!! veilig uit te voeren binnen de mutate
  rlang::inject(
    .data %>%
      dplyr::mutate(!!!.rescolumn := dplyr::case_when(
        !!!formulas,
        .default = .default
      ))
  )
}
