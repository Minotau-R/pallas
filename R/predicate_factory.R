#' @title Generate custom functions based on available predicates.
#' @name predicate_factory
#' @param x `Character scalar`. Names of the desired predicate functions.
#' @param prefix `Character scalar`. Name of the prefix the class is from.
#'     (Default: `""`)
#' @param to.local `Logical scalar`. Whether to return functions in a list
#'     (`TRUE`, Default) or to the environment (`FALSE`).
#' @param vocab.table `data.frame`. Used to customize input hints. Output of
#'     `map_endpoint` with `return.table` set to `TRUE`. (Default: NULL).
#' @param name.prepend `Character scalar`. Prefix for the produced a_class
#'     functions. (Default: `"P."`).
#' @returns A list of functions, or to the environment.
#' @examples
#'
#' P <- predicate_factory(
#'     x = c("predicate_A", "predicate_B", "predicate_C"),
#'     prefix = "a"
#'     )
#'
#' P$P.predicate_A(subject ~ object)
#' P$P.predicate_B(subject ~ object)
#' P$P.predicate_C(subject ~ object)
#'
#' @export
#'
predicate_factory <- function(
        x, prefix = "", to.local = TRUE, vocab.table = NULL, name.prepend = "P."
        ) {
    if(to.local) {
        pred_list <- lapply(
            x, .assign_pred, prefix = prefix, vocab.table = vocab.table
            )
        names(pred_list) <- paste0(name.prepend, x)
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
        hint <- "?subject ~ ?object"
    } else {
    # Else
    force(vocab.table)
    hint <- vocab.table[vocab.table[["property"]] == i, c(1, 2)]
    hint <- paste0(
        "<?", paste0(hint[["range"]], collapse = " "),
        "> ", prefix, ":", i, " <?",
        paste0(hint[["domain"]], collapse = " "),">"
    )
    }

    out_fun <- function(... = "?subject ~ ?object") {
        predicate(..., what = i, prefix = prefix)
    }
    out_fun <- term(out_fun, hint = hint)

    return(out_fun)

}


