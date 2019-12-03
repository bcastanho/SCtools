#' Test if the object is a tdf object
#'
#' This function returns `TRUE` for the object returned from the 
#' \code{generate.placebos} function.
#' and \code{FALSE} for all other objects, including regular data frames.
#'
#' @param x An object
#' @return \code{TRUE} if the object inherits from the `tdf` class.
#' @export
is_tdf <- function(x) {
	inherits(x, "tdf")
}

#' Test if the object is a tdf_multi object
#'
#' This function returns `TRUE` for the object returned from the 
#' \code{multiple.synth} function.
#' and \code{FALSE} for all other objects, including regular data frames.
#'
#' @param x An object
#' @return \code{TRUE} if the object inherits from the `tdf_multi` class.
#' @export
is_tdf_multi <- function(x) {
	inherits(x, "tdf_multi")
}