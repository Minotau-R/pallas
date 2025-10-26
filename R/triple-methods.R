#' @export
#'
S7::method(print, triple) <- function(x, ...) {
    cat(paste0(paste(class(x), collapse = " "), ".\n"))

    cat(x@subject, " ", x@predicate, " ", x@object, "\n")

    invisible(NULL)
}

