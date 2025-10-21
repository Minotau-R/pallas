#' @title Define triple by class, type.
#' @name a_type.name
#' @rdname a_type-methods
#' @description
#'  `a_type()` is a helper function to mimic the `a <Class>` pattern
#' in sparql.
#' While `a_type()` could be called directly, the intended use is to serve as a
#' template to generate prefix-specific functions for each type known by the
#' endpoint.
#' @param x `name`. Likely referring to some variable, i.e., `?some_variable`.
#' @param what `Character scalar`. Which type to use.
#' @param ... additional arguments
#' @importFrom S7 method<- class_name
#' @seealso [predicate.formula()]
#' @returns a `triple` object.
#' @examples
#' # a_type defines a triple:
# a_type(
#     x = var,
#     what = c("prefix", "type")
# )
#' # a_type() handles '?' symbols.
#' a_type(A, what = c("prefix", "type"))
#' a_type(?A, what = c("prefix", "type"))
#'
S7::method(a_type, S7::class_name) <-
    function(x, what) a_type.name(enexpr(x), what)

#' @export
S7::method(a_type, S7::class_language) <-
    function(x, what) a_type.name(enexpr(x), what)

#' @export
S7::method(a_type, S7::class_character) <-
    function(x, what) a_type.character(x, what)

#' @importFrom rlang enexpr
#' @importFrom utils capture.output
a_type.name <- function(x, what) {

    if(is.name(y <- rlang::enexpr(x))) x <- y
    rm(y)

    triple(
        subject = utils::capture.output(x),
        predicate = c("", "a"),
        object = what
        )
}

a_type.character <- function(x, what) triple(
    subject = x,
    predicate = c("", "a"),
    object = what
    )

