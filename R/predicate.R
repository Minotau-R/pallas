#' Define triple by predicate
#' @name predicate.formula
#' @rdname predicate.formula
#' @description While `predicate()` could be called directly, the intended use
#'     is to serve as a template to generate prefix-specific functions for each
#'     predicate known by the endpoint.
#' @param x `formula`. Describing the relationship `subject ~ object`.
#' @param what `Character scalar`. Which predicate to use.
#' @param ... additional arguments
#' @importFrom S7 method<- class_formula
#' @examples
#' predicate(
#'     x = sub_prefix:subject ~ ob_prefix:object,
#'     what = c("prefix", "predicate")
#' )
#' @returns a `triple` object.
#' @export
S7::method(predicate, S7::class_formula) <-
    function(x, what) predicate.formula(x, what)


predicate.formula <- function(x, what) {

    y <- .handleQvar(rlang::enexpr(x))
    if(!isFALSE(y)) x <- y
    rm(y)

    stopifnot("'what' must be of type 'character'." = is.character(what))
    stopifnot("Length of' what' must be 1 or 2." = length(what) %in% c(1L, 2L))


    stopifnot("'x' must be a formula. " = inherits(x, "formula"))
    subject <- if(is.character(x[[2L]])) x[[2L]] else all.vars(x[[2L]])
    object  <- if(is.character(x[[3L]])) x[[3L]] else all.vars(x[[3L]])
    if(! length(subject) %in% c(1L, 2L) ) {
        stop(
            "Issue with left-hand side of 'x' (subject).\n       ",
            "Should be one variable, or two, if connected by ':'. "
        )
    }
    if(! length(object) %in% c(1L, 2L) ) {
        stop(
            "Issue with right-hand side of 'x' (object).\n       ",
            "Should be one variable, or two, if connected by ':'. "
        )
    }
    triple(subject, what, object)
}
