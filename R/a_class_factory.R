#' @title Generate custom `a` functions based on available types.
#' @name a_class_factory
#' @param x `Character vector`. Names of the desired type functions.
#' @param prefix `Character scalar`. Name of the prefix the class is from.
#'     (Default: `""`)
#' @param to.local `Logical scalar`. Whether to return functions in a list
#'     (`TRUE`, Default) or to the environment (`FALSE`).
#' @param name.prepend `Character scalar`. Prefix for the produced predicate
#'     functions. (Default: `"C."`).
#' @returns A list of functions, or to the environment.
#' @examples
#' C <- a_class_factory(x = c("class_A", "class_B", "class_C"))
#'
#' C$C.class_A(?subject)
#' C$C.class_B(subject)
#' C$C.class_C("?subject")
#'
#' @export
#'
a_class_factory <- function(x, prefix = "", to.local = TRUE, name.prepend = "C.") {
    if(to.local) {
        type_list <- lapply(x, .assign_a_class, prefix = prefix)
        names(type_list) <- paste0(name.prepend, x)
        return(type_list)
    } else {
        for (i in x) {
            assign(i, .assign_a_class(i, prefix) )
        }
    }
}

.assign_a_class <- function(i, prefix) {
    force(i)
    force(prefix)
    hint <- paste0("<?var> a ", prefix, ":", i)
    out_fun <- function(... = "?var") {
        a_class(..., what = i, prefix = prefix)
    }
    out_fun <- term(out_fun, hint = hint)
    return(out_fun)
}


