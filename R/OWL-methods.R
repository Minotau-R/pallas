#' @title Methods for `OWL` class.
#' @rdname OWL-methods
#' @name OWL-methods
#' @param x `OWL`.
#' @param pattern `Character scalar`. Pattern to look for.
#' @return the name of the extra data slot
NULL

method(dimnames, OWL) <- function(x) list("term", ls(S7::S7_data(x)))
method(dim, OWL) <- function(x) lengths(dimnames(x))


#' @export
#'
S7::method(print, OWL) <- function(x, ...) {
    cat(
        paste0(paste(class(x), collapse = " "), ".\n")
    )
    cat(x@to_SPARQL, sep = "\n")

    ll <- x@.sparql
    if( length(ll[["prefix"]]) == 0L || identical(ll[["prefix"]], "") ) {
        message(
            "No 'prefix' found. Add one or more prefixes with `add_prefix()`."
            )
    }
    if( length(ll[["query"]]) == 0L || identical(ll[["query"]], "") ) {
        message("No 'query' found. Use `ask_query()` or `select_query()`.")
    }
    if( length(ll[["where"]]) == 0L || identical(ll[["where"]], "")) {
        message("No 'where' found. Add where-clauses with `where_clause()`.")
    }
    invisible(NULL)
}

S7::method(as.SPARQL, OWL) <- function(x) x@to_SPARQL

#' @export
#' @importFrom dplyr tbl_vars group_vars
#'
S7::method(tbl_vars, OWL) <- function(x) `tbl_vars.pallas::OWL`(x)

#' @export
#'
`tbl_vars.pallas::OWL` <- function(x) colnames(x)

#' @export
#'
S7::method(group_vars, OWL) <- function(x) `group_vars.pallas::OWL`(x)

#' @export
#'
`group_vars.pallas::OWL` <- function(x) {
    # Cannot group in OWL context.
    NULL
}


#' @export
#'
S7::method(format, OWL) <- function(x, ...) paste(class(x), collapse = " ")


#' @export
#'
S7::method(.DollarNames, OWL) <- function(
        x, pattern = ""
        ) `.DollarNames.pallas::OWL`(x, pattern)

#' @importFrom utils .DollarNames
#'
`.DollarNames.pallas::OWL` <- function(x, pattern = "") {
    paste0(grep( pattern, colnames(x), value = TRUE ), "()")
}

S7::method(`$`, OWL) <- function(object, name) `[[`(S7::S7_data(object), name)

local({
    S7::method(`[[`, OWL) <- function(x, i) {
        `[[`(S7::S7_data(x), i)
    }

    S7::method(`[`, OWL) <- function(x, i, j, drop = TRUE) {
        if( rlang::is_missing(i) ) {
            if( rlang::is_missing(j) ) { return( x ) } else {
                return( `[[`(x, j) )
            }
        } else {
            return( `[[`(x, i) )
        }
    }
})

#' @export
method(as.character, OWL) <- function(x, ...) {
    ch <- unlist(x@.sparql, use.names = FALSE)
    ch <- ch[ch != ""]
    ch
}

.prefix_to_SPARQL <- function(x) {
    if( is.null(x) ) { return(list()) }
    paste0( "PREFIX ", apply(x, 1L, paste0, collapse = ": <"), ">" )
}

.check_SPARQLvars <- function(x) {
    query_vars <- strsplit(paste0(x@.sparql[["query"]], collapse = " "), " ")
    query_vars <- grep("^\\?", unlist(query_vars), value = TRUE)
    where_vars  <- paste0(
        "?",
        unique(unlist(lapply( x@.sparql[["where"]], names), use.names = FALSE))
        )
    if(!all(query_vars %in% where_vars)) {
        stop(
            "Parameters '",
            paste0(setdiff(query_vars, where_vars), collapse = "', '"),
        "' are mentioned in the query but not found in 'WHERE' clause."
        )

    }
}
