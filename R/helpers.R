#' Safe text extraction from a NeTEx node
#'
#' @param node NeTEx node
#' @param xpath A string containing an xpath (1.0) expression.
#' @param ns 	Optionally, a named vector giving prefix-url pairs, as produced
#' by xml_ns().
#'
#' @returns A character vector, the same length as x.
#' @export
#' @import xml2
#'
gettext_ns <- function(node, xpath, ns = xml_ns(x)) {
  x <- xml_find_first(node, xpath, ns)
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  xml_text(x)
}



#' Get path to a NeTExR example
#'
#' NeTExR comes bundled with a number of sample files in its ‘inst/extdata’
#' directory. This function makes them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#'
#' @export
netexr_example <- function (path = NULL)
{
  if (is.null(path)) {
    dir(system.file("extdata", package = "NeTExR"))
  }
  else {
    system.file("extdata", path, package = "NeTExR", mustWork = TRUE)
  }
}
