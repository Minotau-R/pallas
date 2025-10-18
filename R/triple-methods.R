#' @export
#'
S7::method(print, triple) <- function(x, ...) {
    cat(paste0(paste(class(x), collapse = " "), ".\n"))

    cat(S7::S7_data(x))

    invisible(NULL)
}
