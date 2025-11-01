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
    for( i in seq_along(x@.sparql)) {
        for(t in seq_along(x@.sparql[[i]])) {
            cat(x@.sparql[[i]][[t]], "\n")
        }
    }

    if( length(
        x@.sparql[["prefix"]]) == 0L || identical(x@.sparql[["prefix"]], "") ) {
        message(
            "No 'prefix' found. Add one or more prefixes with `add_prefix()`."
            )
    }
    if( length(
        x@.sparql[["query"]]) == 0L || identical(x@.sparql[["query"]], "")) {
        message("No 'query' found. Add and define a query with `add_query()`.")
    }
    if( length(
        x@.sparql[["where"]]) == 0L || identical(x@.sparql[["where"]], "")) {
        message("No 'where' found. Add where-clauses with `where_clause()`.")
    }
    invisible(NULL)
}


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
   # full_range <- c(colnames(x), paste0("C$",names(x$C)), paste0("P$",names(x$P)))
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

