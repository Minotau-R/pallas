#' @title Methods for `OWL` class.
#' @rdname OWL-methods
#' @name OWL-methods
#' @param x `OWL`.
#' @param pattern `Character scalar`. Pattern to look for.
#' @return the name of the extra data slot
NULL

method(dimnames, OWL) <- function(x) list("term", c("C", "P"))
method(dim, OWL) <- function(x) lengths(dimnames(x))




#' @export
#'
S7::method(print, OWL) <- function(x, ...) {
    cat(
        paste0(paste(class(x), collapse = " "), ".\n")
    )
    print(x@.sparql)

    if( length(x@.sparql[["prefix"]]) == 0L ) {
        message(
            "No 'prefix' found. Add one or more prefixes with `add_prefix()`."
            )
    }
    if( length(x@.sparql[["query"]]) == 0L ) {
        message("No 'query' found. Add and define a query with `add_query()`.")
    }
    if( length(x@.sparql[["where"]]) == 0L ) {
        message("No 'where' found. Add where-clauses with `where_clause()`.")
    }
    invisible(NULL)
}


#' @export
#'
S7::method(tbl_vars, OWL) <- function(x) `tbl_vars.pallas::OWL`(x)

#' @export
#'
`tbl_vars.pallas::OWL` <- function(x) c("C", "P")

#' @export
#'
S7::method(group_vars, OWL) <- function(x) `group_vars.pallas::OWL`(x)

#' @export
#'
`group_vars.pallas::OWL` <- function(x) {
    # Cannot group in OWL context.
    list()
}


#' @export
#'
S7::method(format, OWL) <- function(x, ...) paste0(
    paste(class(x), collapse = " ")
)


#' @export
#'
S7::method(.DollarNames, OWL) <- function(
        x, pattern = ""
        ) `.DollarNames.pallas::OWL`(x, pattern)

#' @importFrom utils .DollarNames
#'
`.DollarNames.pallas::OWL` <- function(x, pattern = "") {
    grep( pattern, colnames(x), value = TRUE )
}

S7::method(`$`, OWL) <- function(object, name) `[[`(object@.env, name)

local({
    S7::method(`[[`, OWL) <- function(x, i) {
        idx <- c("C" = 1, "P" = 2)
        name <- names(idx)[[idx[[i]]]]
        `[[`(x@.env, name)
    }
})


.prefix_to_SPARQL <- function(x) {
    if(is.null(x)) {return(list())}
    paste0( "PREFIX ", apply(x, 1L, paste0, collapse = ": <"), ">" )
}

