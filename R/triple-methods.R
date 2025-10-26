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

