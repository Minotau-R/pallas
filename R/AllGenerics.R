#' Define triple by predicate
#' @name predicate
#' @rdname predicate-generic
#' @description While `predicate()` could be called directly, the intended use
#'     is to serve as a template to generate prefix-specific functions for each
#'     predicate known by the endpoint.
#' @param x object to dispatch on
#' @param ... additional arguments
#' @importFrom S7 new_generic
#' @examples
#' predicate
#' @returns an R object.
#' @export
predicate <- S7::new_generic("predicate", "x")
