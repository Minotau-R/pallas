#' S7 class to contain a SPARQL query.
#' @name SPARQLquery
#' @rdname SPARQLquery-class
#' @description
#' `SPARQLquery` is an S7 class to compose and manage `SPARQL` queries.
#'
#' @slot prefix `Named list of character scalars`, where values are long-form
#'     and names are the corresponding abbreviation.
#' @slot query `Named list` of character vectors, with first element indicating
#'     query form (SELECT, ASK, DESCRIBE), and the second indicating the content
#'      of the query itself.
#' @slot where `List` of triples
#' @param prefix `Named list of character scalars`, where values are long-form
#'     and names are the corresponding abbreviation.
#' @param query `Named list` of character vectors, with first element indicating
#'     query form (SELECT, ASK, DESCRIBE), and the second indicating the content
#'     of the query itself.
#' @param where `List` of triples
#' @returns a `SPARQLquery` object.
#' @importFrom S7 new_class class_list new_property new_object
#' @examples
#' SPARQLquery()
#' @export
#'
SPARQLquery <- S7::new_class(
    "SPARQLquery",
    package = "pallas",
    parent = S7::class_list,
    properties  = list(
        prefix  = S7::new_property(getter = function(self) self[["prefix"]]),
        query   = S7::new_property(getter = function(self) self[["query"]]),
        where   = S7::new_property(getter = function(self) self[["where"]])
    ),
    constructor = function(
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
            prefix  = prefix,
            query   = query,
            where   = where
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
.organiseQvars <- function(x) if(x[1L] == "" && grepl("^\\?", x[2L])) {
    c("?", sub("^\\?", replacement = "", x[2L]))
} else x

