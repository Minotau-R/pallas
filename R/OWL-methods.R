#' @title Methods for `OWL` class.
#' @rdname OWL-methods
#' @name OWL-methods
#' @param x `OWL`.
#' @param pattern `Character scalar`. Pattern to look for.
#' @return the name of the extra data slot
NULL




#' @export
#'
S7::method(print, OWL) <- function(x, ...) {
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
        message("No 'where' found. Add where-clauses with `where_clause()`.")
    }
    invisible(NULL)
}

#' @export
#'
S7::method(.DollarNames, OWL) <- function(
        x, pattern = ""
        ) `.DollarNames.pallas::OWL`(x, pattern)

#' @importFrom utils .DollarNames
#'
`.DollarNames.pallas::OWL` <- function(x, pattern = "") {
    grep( pattern, c("C", "P"), value = TRUE )
}

S7::method(`$`, OWL) <- function(object, name) S7::prop(object, name)

S7::method(length, OWL) <- function(x) length(c("C" = 1, "P" = 2))


local({
    S7::method(`[[`, OWL) <- function(x, i) {
        idx <- c("C" = 1, "P" = 2)
        name <- names(idx)[[idx[[i]]]]
        S7::prop(x, name)
    }
})


.prefix_to_SPARQL <- function(x) {
    if(is.null(x)) {return(list())}
    paste0( "PREFIX ", apply(x, 1L, paste0, collapse = ": <"), ">" )
}

