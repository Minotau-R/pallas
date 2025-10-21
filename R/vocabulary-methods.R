#' @export
#'
S7::method(print, vocabulary) <- function(x, ...) {
    cat(
        paste0(paste(class(x), collapse = " "), ".\n")
    )
    y <- S7::S7_data(x)
    y$prefix <- .prefix_to_SPARQL(y$prefix[["PREFIX"]])
    print(y)
    if( length(x@prefix) == 0L ) {
        message(
            "No 'prefix' found. Add one or more prefixes with `add_prefix()`."
            )
    }
    if( length(x@query) == 0L ) {
        message("No 'query' found. Add and define a query with `add_query()`.")
    }
    if( length(x@where) == 0L ) {
        message("No 'where' found. Add where-clauses with `add_where()`.")
    }
    invisible(NULL)
}

.prefix_to_SPARQL <- function(x) {
    if(is.null(x)) {return(list())}
    paste0( "PREFIX ", apply(x, 1L, paste0, collapse = ": <"), ">" )
}

