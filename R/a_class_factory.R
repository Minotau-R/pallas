#' @title Generate custom `a` functions based on available types.
#' @name a_class_factory
#' @param x `Character scalar`. Names of the desired type functions.
#' @param to.list `Logical scalar`. Whether to return functions in a list
#'     (`TRUE`, Default) or to the environment (`FALSE`).
#' @returns A list of functions, or to the environment.
#' @examples
#' C <- a_class_factory(x = c("class_A", "class_B", "class_C"))
#'
#' C$class_A(?subject)
#' C$class_B(subject)
#' C$class_C("?subject")
#'
#' @export
#'
a_class_factory <- function(x, to.list = TRUE) {
    if(to.list) {
        type_list <- lapply(x, .assign_a_class)
        names(type_list) <- x
        return(type_list)
    } else {
        for (i in x) {
            assign(i, .assign_a_class(i) )
        }
    }
}

.assign_a_class <- function(i) {
    force(i)
    function(... = "?var") {
        a_class(..., what = i)
    }
}


