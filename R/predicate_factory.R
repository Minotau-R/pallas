#' @title Generate custom functions based on available predicates.
#' @name predicate_factory
#' @param x `Character scalar`. Names of the desired predicate functions.
#' @param to.list `Logical scalar`. Whether to return functions in a list
#'     (`TRUE`, Default) or to the environment (`FALSE`).
#' @returns A list of functions, or to the environment.
#' @examples
#' P <- predicate_factory(x = c("predicate_A", "predicate_B", "predicate_C"))
#'
#' P$predicate_A(subject ~ object)
#' P$predicate_B(subject ~ object)
#' P$predicate_C(subject ~ object)
#'
#' @export
#'
predicate_factory <- function(x, to.list = TRUE) {
    if(to.list) {
        pred_list <- lapply(x, .assign_pred)
        names(pred_list) <- x
        return(pred_list)
    } else {
        for (i in x) {
            assign(i, .assign_pred(i) )
        }
    }
}

.assign_pred <- function(i) {
    force(i)
    function(x) predicate.formula(x, what = i)
}


