#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data
NULL

#' grepl a glued regex
#' @description Facilitates using regex in search/filter patterns, and puts the
#'  arguments "the right way round" (x first, then pattern), unlike [grepl()]
#' @returns A logical value
#' @keywords internal
gregg <- \(x, rx, g = parent.frame()) grepl(glue::glue_data(g, rx), x)

#' grepv a glued regex
#' @description Facilitates using regex in search/filter patterns, and puts the
#'  arguments "the right way round" (x first, then pattern), unlike [grepv()]
#' @returns All values of x that match the regex in rx
#' @keywords internal
gregv <- \(x, rx, g = parent.frame()) grepv(glue::glue_data(g, rx), x)
