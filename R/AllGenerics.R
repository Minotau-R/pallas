#' Define triple by predicate
#' @name predicate
#' @rdname predicate-generic
#' @description While `predicate()` could be called directly, the intended use
#'     is to serve as a template to generate prefix-specific functions for each
#'     predicate known by the endpoint.
#' @param x object to dispatch on
#' @param ... additional arguments
#' @importFrom S7 new_generic
#' @examples
#' predicate
#' @returns an R object.
#' @export
predicate <- S7::new_generic("predicate", "x", fun = function(x, ...) {
    y <- .handleQvar(rlang::enexpr(x))
    if(!isFALSE(y)) x <- y
    rm(y)
    S7::S7_dispatch()
})


#' @importFrom rlang enexpr is_function
#' @description We expect formula to start with "?". This function catches and
#'     sorts this out before dispatch.
#' @noRd
#'
.handleQvar <- function(x) {

    if(as.character(x)[1L] == "?") {
        LHS <- paste0("?", rlang::as_label(eval(x[[-1L]])[[2L]]))
        x <- eval(x[[-1L]])
        x[[2L]] <- LHS
        return(x)
    } else return( FALSE )

}

