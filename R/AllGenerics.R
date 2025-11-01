#' Define triple by predicate
#' @name predicate
#' @rdname predicate-generic
#' @description While `predicate()` could be called directly, the intended use
#'     is to serve as a template to generate prefix-specific functions for each
#'     predicate known by the endpoint.
#' @param x object to dispatch on
#' @param ... additional arguments
#' @importFrom S7 new_generic S7_dispatch
#' @importFrom rlang enexpr
#' @examples
#' predicate
#' @returns an R object.
#' @export
predicate <- S7::new_generic("predicate", "x", fun = function(x, ...) {
    x <- rlang::enexpr(x)
    S7::S7_dispatch()
})

#' @title Define triple by class type.
#' @name a_class
#' @rdname a_class-generic
#' @description
#' `a_class()` is a helper function to mimic the `a <Class>` pattern in sparql.
#' While `a_class()` could be called directly, the intended use is to serve as a
#' template to generate prefix-specific functions for each type known by the
#' endpoint.
#' @param x object to dispatch on
#' @param ... additional arguments
#' @importFrom S7 new_generic S7_dispatch
#' @importFrom rlang enexpr
#' @examples
#' a_class
#' @returns an R object.
#' @export
#'
a_class <- S7::new_generic("a_class", "x", fun = function(x, ...) {
    x <- rlang::enexpr(x)
    S7::S7_dispatch()
})

#' @title Define one or more triples for a SPARQL WHERE-clause.
#' @name where_clause
#' @rdname where_clause-queries
#' @param x An object.
#' @param ... `Expression` or sequence of expression separated by comma's, that
#'     each define an RDF triple for the where clause.
#' @returns a `OWL` with the defined expressions included in the `where` slot.
#' @export
#'
where_clause <- S7::new_generic("where_clause", "x")

#' @title Define a SELECT SPARQL command.
#' @name select_query
#' @rdname select_query-queries
#' @param x An object.
#' @param ... `Expression` the SELECT statement.
#' @returns a `OWL` with the defined expressions included in the `query` slot.
#' @export
#'
select_query <- S7::new_generic("select_query", "x")

#' @title Define an ASK SPARQL command.
#' @name select_query
#' @rdname select_query-queries
#' @param x An object.
#' @returns a `OWL` with the defined expressions included in the `query` slot.
#' @export
#'
ask_query <- S7::new_generic("ask_query", "x")

#' @title Coerce to SPARQL query
#' @name as.SPARQL
#' @param x An object.
#' @param ... additional argumants.
#' @returns a `Character scalar` with a SPARQL query.
#' @export
#'
as.SPARQL <- S7::new_generic("as.SPARQL", "x")

method(as.SPARQL, S7::class_any) <- function(x) as.character(x)


#' @description We expect formula to start with "?". This function catches and
#'     sorts this out before dispatch.
#' @noRd
#' @importFrom rlang as_label
#'
.defuseqLHS <- function(x) {

    if(as.character(x)[1L] == "?") {
        LHS <- paste0("?", rlang::as_label(eval(x[[-1L]])[[2L]]))
        x <- eval(x[[-1L]])
        x[[2L]] <- LHS
        return(x)
    } else return( FALSE )

}

#' @noRd
#' @param rlang as_label is_symbol
#'
.qRHS <- function(x) {
    if(rlang::is_symbol(x[[3L]])) { return(x) }
    if(as.character(x[[3L]][1L]) == "?") {
        RHS <- paste0("?", rlang::as_label(x[[3L]][[2L]]))
        x[[3L]] <- RHS
    }
    return(x)
}
