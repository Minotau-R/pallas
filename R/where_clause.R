#' @title Define one or more triples for a SPARQL WHERE-clause.
#' @name where_clause.OWL
#' @rdname building-queries
NULL

#' @importFrom S7 method<-
S7::method(where_clause, OWL) <-
    function(x, ..., .append = TRUE, .drop.empty = TRUE) .edit_sparql(
        x, section = "where", .append = .append, .drop.empty = .drop.empty, ...
    )

#' @importFrom S7 method<-
S7::method(select_query, OWL) <-
    function(x, ..., .append = FALSE, .drop.empty = TRUE) .edit_sparql(
        x, section = "query", .append = .append, .drop.empty = .drop.empty, ...
    )

#' @importFrom S7 method<-
S7::method(ask_query, OWL) <-
    function(x, .append = FALSE, .drop.empty = TRUE) .edit_sparql(
        x, section = "query", .append = .append, .drop.empty = .drop.empty, "ASK"
    )

#' @importFrom rlang exprs
.edit_sparql <- function(x, section, .append, .drop.empty, ...) {
    e <- lapply(rlang::exprs(...), eval, envir = S7::S7_data(x))
    q <- c( if(.append) {x@.sparql[[section]] }, e)
    #q <- c( x@.sparql[[section]], rlang::dots_list(...) )
    if(.drop.empty) q <- q[ q != "" ]
    x@.sparql[[section]] <- q
    x
}

