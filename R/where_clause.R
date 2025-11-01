#' @title Define one or more triples for a SPARQL WHERE-clause.
#' @name where_clause.OWL
#' @rdname building-queries
NULL

#' @importFrom S7 method<-
S7::method(where_clause, OWL) <- function(x, ..., .append = TRUE, .drop.empty = TRUE) {
    .edit_sparql(x, section = "where", .append = .append, .drop.empty = .drop.empty, ...)
    }

#' @importFrom rlang exprs
.edit_sparql <- function(x, section, .append, .drop.empty, ...) {
    e <- lapply(rlang::exprs(...), eval, envir = S7::S7_data(x))
    q <- c( if(.append) {x@.sparql[[section]] }, e)
    #q <- c( x@.sparql[[section]], rlang::dots_list(...) )
    if(.drop.empty) q <- q[ q != "" ]
    x@.sparql[[section]] <- q
    x
}

.OWL_env <- function(e) {
    print(e)
    env_calls <- vapply(e, .is_env_call, FALSE)
    if(any(env_calls)) {

    e[env_calls] <- lapply(
        e[env_calls], function(z) str2expression(paste0("x$", deparse1(z)))
        )
    }
}

.is_env_call <- function(x) {
    y <- x[[1L]]
    print(y)

    if(is_call(y, name = "$")) {
        return( as.character(y[[2L]]) %in% c("C", "P") )
    }
    FALSE
}
