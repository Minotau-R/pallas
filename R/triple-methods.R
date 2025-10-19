#' @export
#'
S7::method(print, triple) <- function(x, ...) {
    cat(paste0(paste(class(x), collapse = " "), ".\n"))

    cat(S7::S7_data(x))

    invisible(NULL)
}


#' Define triple by predicate
#' @name predicate.triple
#' @aliases predicate.pallas::triple
#' @rdname triple-methods
#' @param x `triple`.
#' @examples
#' x <- triple()
#' predicate(x)
#' @returns `predicate()`: The predicate slot of input `triple` `'x'`.
NULL

#' @export
S7::method(predicate, triple) <- function(x) `predicate.pallas::triple`(x)

`predicate.pallas::triple` <- function(x) x@predicate
