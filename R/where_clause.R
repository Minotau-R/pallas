#' @title Define one or more triples for a SPARQL WHERE-clause.
#' @name where_clause
#' @rdname building-queries
#' @param x `vocabulary` object.
#' @param ... `Expression` or sequence of expression separated by comma's, that
#'     each define an RDF triple for the where clause.
#' @returns a `vocabulary` with the defined expressions included in the `where`
#'     slot.
#' @export
#'
where_clause <- function(x, ...) {
    if( length(x[["where"]]) != 0L) {
        within(
            x,
            where <- c(where, list(...))
        )
    } else {
        within(
            x,
            where <- list(...)
        )
    }
}
