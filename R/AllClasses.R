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
#' @export
#'
SPARQLquery <- S7::new_class(
    "pallas",
    package = "SPARQLquery",
    parent = S7::class_data.frame,
    properties  = list(
        prefix  = S7::class_list,
        query   = S7::class_list,
        where   = S7::class_list
    ),
    constructor = function(
        prefix = list(),
        query = list(),
        where = list()
        ) {

        S7::new_object(
            .parent = S7::S7_object(),
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
#' @export
#'
triple <- S7::new_class(
    "pallas",
    package = "triple",
    parent = S7::class_data.frame,
    properties = list(
        subject   = S7::class_character,
        predicate = S7::class_character,
        object    = S7::class_character
    ),
    constructor = function(
        subject = c(),
        predicate = c(),
        object = c()
        ) {

        S7::new_object(
            .parent   = S7::S7_object(),
            subject   = subject,
            predicate = predicate,
            object    = object
        )
    }
)

