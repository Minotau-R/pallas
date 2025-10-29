#' @export
#'
S7::method(print, term) <- function(x, ...) {
   cat("  ", x@hint )
}

#' @export
#'
S7::method(format, term) <- function(x, ...) paste0(
    paste(class(x), collapse = " ")
    )
