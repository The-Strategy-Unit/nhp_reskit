#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data .env
NULL

#' grepl a glued regex
#' @description Facilitates using regex in search/filter patterns, and puts the
#'  arguments "the right way round" (x first, then pattern), unlike [grepl]
#' @param x The character vector to search within
#' @param rx A regular expression (pattern) to search for
#' @param g The environment within which `rx` is evaluated
#' @returns A logical vector
#' @keywords internal
gregg <- \(x, rx, g = parent.frame()) grepl(glue::glue_data(g, rx), x)

#' grepv a glued regex
#' @description Facilitates using regex in search/filter patterns, and puts the
#'  arguments "the right way round" (x first, then pattern), unlike [grepv]
#' @returns A character vector: all values of x that match the regex in rx
#' @keywords internal
#' @rdname gregg
gregv <- \(x, rx, g = parent.frame()) grepv(glue::glue_data(g, rx), x)
