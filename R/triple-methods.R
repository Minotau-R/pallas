#' @export
#'
S7::method(print, triple) <- function(x, ...) {
    cat(paste0(paste(class(x), collapse = " "), ".\n"))

    cat(S7::S7_data(x), "\n")

    invisible(NULL)
}

#' Define triple by predicate
#' @name predicate.triple
#' @aliases predicate.pallas::triple
#' @rdname triple-methods
#' @param x `triple`.
#' @returns `predicate()`: The predicate slot of input `triple` `'x'`.
#' @examples
#' x <- triple(
#'     subject = c("sub_prefix", "subject"),
#'     predicate = c("pred_prefix", "predicate"),
#'     object = c("obj_prefix", "object")
#' )
#' predicate(x)
#'
NULL

#' @export
S7::method(predicate, triple) <- function(x) `predicate.pallas::triple`(x)

`predicate.pallas::triple` <- function(x) x@predicate
