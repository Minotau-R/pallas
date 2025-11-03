#' @export
#'
S7::method(print, triple) <- function(x, ...) {
    cat(paste0(paste(class(x), collapse = " "), ".\n"))
    dx <- S7::S7_data(x)
    nx <- names(dx)
    len <- pmax(nchar(dx) , nchar(nx))

    for(i in seq_along(len)) {
        cat(format(nx[i], width = len[i]), " ")
    }
    cat("\n")
    for(i in seq_along(len)) {
        cat(format(dx[i], width = len[i], justify = "right"), " ")
    }
    cat("\n")
    invisible(NULL)
}

#' @importFrom S7 method<- S7_data
S7::method(as.SPARQL, triple) <-
    function(x) paste0(S7::S7_data(x), collapse = " ")


.triple_vars <- function(x) {
    s <- if(x@subject[1] == "?") x@subject[2] else NULL
    p <- if(x@predicate[1] == "?") x@predicate[2] else NULL
    o <- if(x@object[1] == "?") x@object[2] else NULL
    return( unique(c(s, p, o)) )
}

.triple_prefs <- function(x) {
    s <- if(any(x@subject %in% c("", "?"))) NULL else x@subject[1]
    p <- if(any(x@predicate %in% c("", "?"))) NULL else x@predicate[1]
    o <- if(any(x@object %in% c("", "?"))) NULL else x@object[1]
    return( unique(c(s, p, o)) )
}
