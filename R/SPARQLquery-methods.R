#' @export
#'
S7::method(print, SPARQLquery) <- function(x, ...) {
    cat(
        paste0(paste(class(x), collapse = " "), ".\n")
    )
    print(S7::S7_data(x))
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
