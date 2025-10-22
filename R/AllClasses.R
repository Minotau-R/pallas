#' S7 class to contain a SPARQL query.
#' @name vocabulary
#' @rdname vocabulary-class
#' @description
#' `vocabulary` is an S7 class to compose and manage `SPARQL` queries.
#' @slot lexicon `Named list`. Contains convenience functions, for instance
#'     those generated from `map_endpoint()`
#' @slot prefix `Named list of character scalars`, where values are long-form
#'     and names are the corresponding abbreviation.
#' @slot query `Named list` of character vectors, with first element indicating
#'     query form (SELECT, ASK, DESCRIBE), and the second indicating the content
#'      of the query itself.
#' @slot where `List` of triples
#' @param lexicon `Named list`. Contains convenience functions, for instance
#'     those generated from `map_endpoint()`
#' @param prefix `Named list of character scalars`, where values are long-form
#'     and names are the corresponding abbreviation.
#' @param query `Named list` of character vectors, with first element indicating
#'     query form (SELECT, ASK, DESCRIBE), and the second indicating the content
#'     of the query itself.
#' @param where `List` of triples
#' @returns a `vocabulary` object.
#' @importFrom S7 new_class class_list new_property new_object
#' @examples
#' vocabulary()
#' @export
#'
vocabulary <- S7::new_class(
    "vocabulary",
    package = "pallas",
    parent = S7::class_list,
    properties  = list(
        lexicon = S7::class_list,
        P = S7::new_property(getter = function(self) self@lexicon$predicates),
        C = S7::new_property(getter = function(self) self@lexicon$classes),

        prefix  = S7::new_property(getter = function(self) self[["prefix"]]),
        query   = S7::new_property(getter = function(self) self[["query"]]),
        where   = S7::new_property(getter = function(self) self[["where"]])
    ),
    constructor = function(
        lexicon = list(),
        prefix = list(),
        query = list(),
        where = list()
    ) {
        x <- list(
            prefix = prefix,
            query = query,
            where = where
        )
        S7::new_object(
            .parent = x,
            lexicon = lexicon
                        )
    }
)

#' S7 class to contain an RDF triple.
#' @name triple
#' @rdname triple-class
#' @description
#' `triple` is an S7 class contain RDF triples that make up a SPARQL query.
#' A `triple` expression describes a relationship in three parts, known as
#' `subject–predicate–object` expression. In the data class, this is reflected
#' by its three slots.
#'
#' @slot subject `Character vector`. Length one or two. First position gives
#'     subject name, second (optional, default `NULL`) gives prefix.
#' @slot predicate `Character scalar`. Length one or two. First position gives
#'     predicate name, second (optional, default `NULL`) gives prefix.
#' @slot object `Character scalar`. Length one or two. First position gives
#'     object name, second (optional, default `NULL`) gives prefix.
#' @param subject `Character vector`. Length one or two. First position gives
#'     subject name, second (optional, default `NULL`) gives prefix.
#' @param predicate `Character scalar`. Length one or two. First position gives
#'     predicate name, second (optional, default `NULL`) gives prefix.
#' @param object `Character scalar`. Length one or two. First position gives
#'     object name, second (optional, default `NULL`) gives prefix.
#' @returns a `triple` object.
#' @importFrom S7 class_character new_property S7_data
#' @examples
#' triple(
#'     subject = c("sub_prefix", "subject"),
#'     predicate = c("pred_prefix", "predicate"),
#'     object = c("obj_prefix", "object")
#' )
#' @export
#'
triple <- S7::new_class(
    "triple",
    package = "pallas",
    parent = S7::class_character,
    properties = list(
        subject   = S7::new_property(
            getter = function(self) S7::S7_data(self)[c(1L, 2L)]
        ),
        predicate = S7::new_property(
            getter = function(self) S7::S7_data(self)[c(3L, 4L)]
        ),
        object    = S7::new_property(
            getter = function(self) S7::S7_data(self)[c(5L, 6L)]
        )
    ),
    constructor = function(
        subject = c("", ""), predicate = c("", ""), object = c("", "")
    ) {
        stopifnot(
            "Args must be length one: c('name') or two: c('prefix', 'name')." =
                length(subject) %in% c(1L, 2L) &&
                length(predicate) %in% c(1L, 2L) &&
                length(object) %in% c(1L, 2L)
        )
        if(length(subject) == 1L) { subject <- c("", subject) }
        subject <- .organiseQvars(subject)

        if(length(predicate) == 1L) { predicate <- c("", predicate) }
        predicate <- .organiseQvars(predicate)

        if(length(object) == 1L) { object <- c("", object) }
        object <- .organiseQvars(object)

        x <- c( subject[c(1L, 2L)], predicate[c(1L, 2L)], object[c(1L, 2L)] )
        x[is.na(x)] <- ""
        S7::new_object( .parent = x )
    }
)

#' @param x `Character vector`. subject, predicate or object of a triple.
#' @noRd
.organiseQvars <- function(x) {
    if( x[1L] != "" ) {
        return( x )
    }
    if( grepl("^\\?", x[2L]) ) {
        return( c("?", sub("^\\?", replacement = "", x[2L])) )
    }
    if( grepl("^.*:.*$", x[2L]) ) {
        x <- unlist(strsplit(x[2L], split = ":", fixed = TRUE), FALSE, FALSE)
        if(length(x) != 2L) {
            stop(
                "Triple args 'subject', 'predicate', 'object'\n    ",
                "should each only contain one ':' character at most."
            )
        }
    }
    return( x )
}



