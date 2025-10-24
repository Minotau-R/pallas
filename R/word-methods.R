#' @export
#'
S7::method(print, term) <- function(x, ...) {
   cat( x@hint )
}
