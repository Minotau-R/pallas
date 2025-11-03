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
    .check_SPARQL(x)

    invisible(NULL)
}

.check_SPARQL <- function(x) {
    ll <- x@.sparql
    no_p <-  length(ll[["prefix"]]) == 0L || identical(ll[["prefix"]], "")
    no_q <-  length(ll[["query"]]) == 0L || identical(ll[["query"]], "")
    no_w <-  length(ll[["where"]]) == 0L || identical(ll[["where"]], "")
    if(no_p) message(
        "No 'prefix' found. Add one or more prefixes with `add_prefix()`."
    )
    if(no_q) message(
        "No 'query' found. Use `ask_query()` or `select_query()`."
    )
    if(no_w) message(
        "No 'where' found. Add where-clauses with `where_clause()`."
    )
    if(!no_w) {
        if(!is.null(var_res <- .check_SPARQLvars(x)))  { message(var_res) }
        if(!no_p) {
            if(!is.null(prf_res <- .check_SPARQLprefs(x))) { message(prf_res) }
        }
    }
    if(!no_q) {
        if(!is.null(qry_res <- .check_SPARQLquery(x))) { message(qry_res) }
    }

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
    if(identical(.get_query(x)[1L], "ASK")) { return(invisible()) }
    query_vars <- .get_query_vars(x)
    if(identical(query_vars, "*")) { return(invisible()) }
    where_vars <- .get_where_vars(x)

    if(!all(query_vars %in% where_vars)) {
        return(
            paste0(
                "Parameters '",
                paste0(setdiff(query_vars, where_vars), collapse = "', '"),
                "' are mentioned in the query but not found in 'WHERE' clause."
            )
        )
    } else { return( invisible(NULL) ) }
}

.check_SPARQLprefs <- function(x) {
    defined_prefs <- .get_defined_prefs(x)

    where_prefs <- .get_where_prefs(x)

    if(!all(where_prefs %in% defined_prefs)) {
        return(
            paste0(
                "Undefined prefixes '",
                paste0(setdiff(where_prefs, defined_prefs), collapse = "', '"),
                "' found in 'WHERE' clause."
            )
        )
    } else { return( invisible(NULL) ) }
}

.get_query_vars <- function(x) {
    query_vars <- strsplit(paste0(x@.sparql[["query"]], collapse = " "), " ")
    query_vars <- grep("^\\?|^\\*$", unlist(query_vars), value = TRUE)
    return(query_vars)
}

.get_where_vars <- function(x) {
    where_vars <- lapply( x@.sparql[["where"]], .triple_vars)
    where_vars <- paste0( "?", unique(unlist(where_vars, use.names = FALSE)) )
    return(where_vars)
}

.check_SPARQLquery <- function(x) {
    query <- .get_query(x)
    q_type <- query[1]
    s <- c("SELECT", "ASK", "DESCRIBE", "CONSTRUCT")
    if( !toupper(q_type) %in% s ) {
        return(
            paste0(
                "Query type '", q_type,
                "' not recognised. \nValid options include: '",
                paste0(s, collapse = "', '"),
                "."
            )
        )
    }
    if(q_type == "ASK" && length(query) > 1L) {
        return("SPARQL queries of type 'ASK' do not take variable arguments.")
    }
    if(q_type %in% c("SELECT", "DESCRIBE") && length(query) == 1L) {
        return(
            paste0(
                "SPARQL queries of type '", q_type,
                "' require at least one ?variable defined."
            )
        )
    }
    return( invisible( NULL ) )
}

.get_query <- function(x) {
    q <- paste0(unlist(x@.sparql[["query"]], use.names = FALSE), collapse = " ")
    q <- unlist(strsplit(q, split = " ", fixed = TRUE))
    return(q)
}

.get_defined_prefs <- function(x) {
    defined_prefs <- x@.env[["prefixes"]][["short"]]
    return(defined_prefs)
}

.get_where_prefs <- function(x) {
    where_prefs <- lapply( x@.sparql[["where"]], .triple_prefs)
    where_prefs <- unique(unlist(where_prefs, use.names = FALSE))
    return(where_prefs)
}
