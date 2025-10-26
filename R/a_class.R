#' @title Define triple by class, type.
#' @name a_class.name
#' @rdname a_class-methods
#' @description
#'  `a_class()` is a helper function to mimic the `a <Class>` pattern
#' in sparql.
#' While `a_class()` could be called directly, the intended use is to serve as a
#' template to generate prefix-specific functions for each type known by the
#' endpoint.
#' @param x `name`. Likely referring to some variable, i.e., `?some_variable`.
#' @param what `Character scalar`. Which type to use.
#' @param prefix `Character scalar`. Name of the prefix the class is from.
#'     (Default: `""`)
#' @param ... additional arguments
#' @importFrom S7 method<- class_name
#' @seealso [predicate-methods]
#' @returns a `triple` object.
#' @examples
#' # a_class defines a triple:
#  a_class(x = var, what = "type", prefix = "prefix")
#
#' # a_class() handles '?' symbols.
#' a_class(A, what = "type", prefix = "p")
#' a_class(?A, what = "type", prefix = "p")
#' a_class(p_A:A, what = "type", prefix = "p")
NULL

#' @export
#' @importFrom S7 method<- class_call method
S7::method(a_class, S7::class_call) <- function(x, what = "", prefix = "") {
    x <- .Q2var(substitute(x))
    x <- .colon2var(x)
    triple(
        subject = as.character(x),
        predicate = c("", "a"),
        object = c(prefix, what)
    )
}


#' @export
S7::method(a_class, S7::class_name) <- function(x, what, prefix = "")
        a_class.name(rlang::enexpr(x), what, prefix)

#' @export
S7::method(a_class, S7::class_character) <-
    function(x, what, prefix = "") a_class.character(x, what, prefix)

#' @importFrom rlang enexpr
#' @importFrom utils capture.output
a_class.name <- function(x, what, prefix = "") {

    x <- .Q2var(x)
    triple(
        subject = utils::capture.output(x),
        predicate = c("", "a"),
        object = c(prefix, what)
    )
}

a_class.character <- function(x, what, prefix = "") triple(
    subject = x,
    predicate = c("", "a"),
    object = c(prefix, what)
)

.colon2var <- function(x) {
    if(is.call(x)) {
        if( as.character(x)[1L] %in% c(":", "c", "~") ) {
            x1 <- as.character(x)[2L]
            x2 <- as.character(x)[3L]

            if( is.na(x2) ) {
                x2 <- x1
                x1 <- ""
            }
            x <- c( x1, x2 )

        }
    }
    return(x)
}

