#' @title Column names for RStudio auto-complete
#' @name tbl_vars.OWL_env
#' @aliases tbl_vars.OWL_env
#' @description
#' Returns the names OWL_env content. Needed for RStudio to complete variable
#' names
#' @param x `OWL_env`.
#' @returns The names of content within `OWL_env` object `x`.
#' @importFrom rlang is_missing
NULL


method(dimnames, OWL_env) <- function(x) list("term", names(x))
method(dim, OWL_env) <- function(x) lengths(dimnames(x))
method(`print`, OWL_env) <- function(x, ...) print(S7::S7_data(x), ...)

local({
    method(`[[`, OWL_env) <- function(x, i) `[[`(S7::S7_data(x), i)
    method(`[`, OWL_env) <- function(x, i, j, drop = TRUE) {
        if(rlang::is_missing(i)) {
            if(rlang::is_missing(j)) { return(x) } else {
                return( `[[`(S7::S7_data(x), j) )
            }
        } else {
            return( `[[`(S7::S7_data(x), i) )
        }
    }
})

#' @export
#'
S7::method(tbl_vars, OWL_env)  <- function(x) `tbl_vars.pallas::OWL_env`(x)

#' @export
#' @importFrom dplyr tbl_vars group_vars
#'
`tbl_vars.pallas::OWL_env` <- function(x) {
    names(x)
}

#' @export
#' @importFrom dplyr tbl_vars group_vars
#'
S7::method(group_vars, OWL_env) <- function(x) `group_vars.pallas::OWL_env`(x)

#' @export
#'
`group_vars.pallas::OWL_env` <- function(x) {
    # Cannot group in OWL_env context.
    list()
}


#' @export
#'
S7::method(.DollarNames, OWL_env) <- function(
        x, pattern = ""
) `.DollarNames.pallas::OWL`(x, pattern)

#' @importFrom utils .DollarNames
#'
`.DollarNames.pallas::OWL_env` <- function(x, pattern = "") {
    grep( pattern, colnames(x), value = TRUE )
}

S7::method(`$`, OWL_env) <- function(object, name) `[[`(object, name)
