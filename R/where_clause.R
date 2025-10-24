#' @title Define one or more triples for a SPARQL WHERE-clause.
#' @name where_clause.vocabulary
#' @rdname building-queries
#'
NULL

#' @importFrom S7 method<- class_missing
S7::method(where_clause, S7::class_missing) <- function(x, ...) {
    vocabulary(where = list(...))
    }

#' @importFrom S7 method<- S7_data<-
S7::method(where_clause, vocabulary) <- function(x, ..., append = TRUE) {
    where <- if(append) S7::S7_data(x)[["where"]] else NULL
    where <- c( where, list(...) )

    new_x <- S7::S7_data(x)
    new_x <- `[[<-`(new_x , "where", value = unlist(where))
    x <- S7::`S7_data<-`( x, value =  new_x)
    return(x)
    }
