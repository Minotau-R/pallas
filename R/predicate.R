#' Define triple by predicate
#' @name predicate.formula
#' @rdname predicate.formula
#' @description While `predicate()` could be called directly, the intended use
#'     is to serve as a template to generate prefix-specific functions for each
#'     predicate known by the endpoint.
#' @param x `formula`. Describing the relationship `subject ~ object`.
#' @param what `Character scalar`. Which predicate to use.
#' @param prefix `Character scalar`. Name of the prefix the class is from.
#'     (Default: `""`)
#' @param ... additional arguments
#' @importFrom S7 method<- class_formula
#' @returns a `triple` object.
#' @examples
#' # predicate defines a triple:
#' predicate(
#'     x = sub_prefix:subject ~ ob_prefix:object,
#'     what = "predicate",
#'     prefix = "prefix"
#'     )
#'
#' # predicate() handles '?' symbols (Unlike regular formula objects).
#' predicate(a:b~c:d, what = "", prefix = "pa")
#' predicate( ?b~c:d, what = "", prefix = "b")
#' predicate(a:b~ ?d, what = "", prefix = "c")
#' predicate( ?b~ ?d, what = "", prefix = "d")
#'
S7::method(predicate, S7::class_formula) <-
    function(x, what, prefix = "") predicate.formula(x, what, prefix)


predicate.formula <- function(x, what, prefix = "") {

    y <- .defuseqLHS(rlang::enexpr(x))
    if(!isFALSE(y)) x <- y
    rm(y)
    x <- .qRHS(x)

    stopifnot("'what' must be of type 'character'." = is.character(what))
    stopifnot("Length of' what' must be 1 or 2." = length(what) %in% c(1L, 2L))
    if(length(what) == 1L) what <- c(prefix, what)

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


