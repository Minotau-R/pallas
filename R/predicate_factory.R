#' @title Generate custom functions based on available predicates.
#' @name predicate_factory
#' @param x `Character scalar`. Names of the desired predicate functions.
#' @param prefix `Character scalar`. Name of the prefix the class is from.
#'     (Default: `""`)
#' @param to.list `Logical scalar`. Whether to return functions in a list
#'     (`TRUE`, Default) or to the environment (`FALSE`).
#' @param vocab.table `data.frame`. Used to customize input hints. Output of
#'     `map_endpoint` with `return.table` set to `TRUE`. (Default: NULL).
#' @returns A list of functions, or to the environment.
#' @examples
#'
#' P <- predicate_factory(
#'     x = c("predicate_A", "predicate_B", "predicate_C"),
#'     prefix = "a"
#'     )
#'
#' P$predicate_A(subject ~ object)
#' P$predicate_B(subject ~ object)
#' P$predicate_C(subject ~ object)
#'
#' @export
#'
predicate_factory <- function(
        x, prefix = "", to.list = TRUE, vocab.table = NULL
        ) {
    if(to.list) {
        pred_list <- lapply(
            x, .assign_pred, prefix = prefix, vocab.table = vocab.table
            )
        names(pred_list) <- x
        return(pred_list)
    } else {
        for (i in x) {
            assign(i, .assign_pred(i, prefix, vocab.table) )
        }
    }
}


#' @importFrom rlang fn_fmls<-
#' @importFrom stats reformulate
.assign_pred <- function(i, prefix, vocab.table) {
    force(i)
    force(prefix)
    if(is.null(vocab.table)) {
        return (
            function(... = subject ~ object ) {
                predicate.formula(..., what = i, prefix = prefix)
            }
        )
    }
    # Else
    force(vocab.table)
    hint <- vocab.table[vocab.table[["property"]] == i, c(1, 2)]
    hint <- reformulate(
        paste0(hint[["range"]], collapse = " "),
        paste0(hint[["domain"]], collapse = " ")
    )
    # Return a wrapper around predicate.formula.
    # Adjust formals to contain hint.
    rlang::`fn_fmls<-`(
        function(... = subject ~ object ) {
            predicate.formula(..., what = i, prefix = prefix)
        },
        value = list("..." = hint)
    )
}


