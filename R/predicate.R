#' Define triple by predicate
#' @name predicate-methods
#' @rdname predicate-methods
#' @description While `predicate()` could be called directly, the intended use
#'     is to serve as a template to generate prefix-specific functions for each
#'     predicate known by the endpoint.
#' @param x `formula`. Describing the relationship `subject ~ object`.
#' @param what `Character scalar`. Which predicate to use.
#' @param prefix `Character scalar`. Name of the prefix the class is from.
#'     (Default: `""`)
#' @param ... additional arguments
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
NULL


#' @export
#' @importFrom S7 method<- class_call method
S7::method(predicate, S7::class_call) <- function(x, what = "", prefix = "") {
    x <- predicate.call(substitute(x))
    # Call formula method
    (S7::method(predicate, object = x))(x, what, prefix)
}

predicate.call <- function(x)  {
    x_1 <- as.character(x[[1L]])
    stopifnot("Input arg 'x' cannot be of length 0." = length(x_1) != 0L)

    # Content is a vector:
    if(x_1 == "c") {
        x[[1L]] <- rlang::expr(`~`)
        x_1 <- as.character(x[[1L]])
    }

    # Starting with "?"
    if(x_1 == "?") {
        x <- x[[-1L]]
        x[[2L]] <-  str2lang(paste0("?", x[[2L]]))
        x_1 <- as.character(x[[1L]])
    }
    x <- eval(x, envir = NULL)
    return(x)
}

#' @importFrom S7 method<- class_formula
#' @export
#'
S7::method(predicate, S7::class_formula) <-
    function(x, what = "", prefix = "") predicate.formula(x, what, prefix)



predicate.formula <- function(x, what = "", prefix = "") {
    x <- .Qs2var(x)

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


#' @noRd
#' @description
#' Handles variables that start with a ?.
#' @param x formula
.Qs2var <- function(x) {
    stopifnot("Input should be a formula." = inherits(x, "formula"))
    LHS <- .Q2var(x[[2L]])
    RHS <- .Q2var(x[[3L]])
    do.call("~", c(LHS, RHS))
}

.Q2var <- function(x) {
    if(is.call(x)) {
        if(as.character(x)[[1L]] == "?") {
            x <- as.symbol(paste0("?", x[[-1L]]))
        }
    }
    return(x)
}

