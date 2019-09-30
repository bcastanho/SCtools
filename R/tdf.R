#' Test if the object is a 
#'
#' This function returns `TRUE` for the object returned from the 
#' \code{generate.placebos} function.
#' and `FALSE` for all other objects, including regular data frames.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `tdf` class.
#' @export
is_tdf <- function(x) {
	inherits(x, "tdf")
}

