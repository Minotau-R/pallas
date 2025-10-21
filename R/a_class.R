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
#' @seealso [predicate.formula()]
#' @returns a `triple` object.
#' @examples
#' # a_class defines a triple:
#  a_class(x = var, what = "type", prefix = "prefix")
#
#' # a_class() handles '?' symbols.
#' a_class(A, what = "type", prefix = "p")
#' a_class(?A, what = "type", prefix = "p")
#' a_class(p_A:A, what = "type", prefix = "p")
#'
S7::method(a_class, S7::class_name) <-
    function(x, what, prefix = "") a_class.name(enexpr(x), what, prefix)

#' @export
S7::method(a_class, S7::class_language) <-
    function(x, what, prefix = "") a_class.name(enexpr(x), what, prefix)

#' @export
S7::method(a_class, S7::class_character) <-
    function(x, what, prefix = "") a_class.character(x, what, prefix)

#' @importFrom rlang enexpr
#' @importFrom utils capture.output
a_class.name <- function(x, what, prefix = "") {

    if(is.name(y <- rlang::enexpr(x))) x <- y
    rm(y)

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

