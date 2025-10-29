#' S7 class to contain an OWL environment.
#' @name OWL_env
#' @rdname OWL_env-class
#' @description
#' `OWL_env` is an S7 class which is typically slotted into `OWL` objects to
#' serve as a context, an environment, in which to keep track of the "non-SPARQL
#' query code" parts of the `OWL` object. This includes custom triple generating
#' functions (`term`s), but also the variables named in the query so far.
#'
#' @param x `List` object.
#' @returns an `OWL_env` object.
#' @importFrom S7 class_character new_property new_object S7_data
#' @examples
#' OWL_env
#'
#' @export
#'
OWL_env <- S7::new_class(
    "OWL_env",
    package = "pallas",
    parent = S7::new_S3_class("tbl"),

    constructor = function( x ) {
        S7::new_object(
            .parent = x
        )
    }
)

#' S7 class to contain a SPARQL query.
#' @name OWL
#' @rdname OWL-class
#' @description
#' `OWL` is an S7 class to compose and manage `SPARQL` queries.
#' @slot .env `Named list`. Contains convenience functions, for instance
#'     those generated from `map_endpoint()`
#' @slot sparql. `Named list`. Contains SPARQL code in three sections:
#'     `prefix`, `query` and `where`.
#' @param .env `Named list`. Contains convenience functions, for instance
#'     those generated from `map_endpoint()`
#' @param prefix `Named list of character scalars`, where values are long-form
#'     and names are the corresponding abbreviation.
#' @param query `Named list` of character vectors, with first element indicating
#'     query form (SELECT, ASK, DESCRIBE), and the second indicating the content
#'     of the query itself.
#' @param where `List` of triples
#' @returns a `OWL` object.
#' @importFrom S7 new_class class_list new_property new_object S7_data
#' @examples
#' OWL()
#' @export
#'
OWL <- S7::new_class(
    "OWL",
    package = "pallas",
    parent = S7::new_S3_class("tbl"),
    properties  = list(
        .env = S7::class_list,
        .sparql = S7::class_list
    ),
    constructor = function(
        .env = list(
            P = OWL_env(list()),
            C = OWL_env(list()),
            V = character(0L),
            prefixes = data.frame()
        ),
        prefix = "",
        query = "",
        where = ""
    ) {
        x <- data.frame(P = "", C = "")

        .env <- .env[c("P", "C", "V", "prefixes")]
        S7::new_object(
            .parent = x,
            .env = .env,
            .sparql = list(
                prefix = prefix,
                query = query,
                where = where
            )
        )
    },
    validator = function(self) {
        # .self <- S7::S7_data(self)
        # ll <- vapply(.self, is.vector, FALSE, USE.NAMES = TRUE)
        #
         if(!identical(names(self@.sparql), c("prefix", "query", "where")) ) {
             "`.sparql` slot should only contain 'prefix', 'query' and 'where'."
         }
        # if(!all(ll)){
        #     paste0("Content of '", paste0(names(ll)[!ll], collapse = "', '"),
        #            "' must inherit from vector.")
        # }
        if( !all(names(self@.env) %in% c("C", "P", "prefixes", "V")) ) {
            "names of '.env' must contain exactly c('C', 'P', 'prefixes', 'V')."
        }
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
        subject   = S7::new_property( getter = function(self) self@subject ),
        predicate = S7::new_property( getter = function(self) self@predicate ),
        object    = S7::new_property( getter = function(self) self@object )
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

        x <- vapply(
            list(subject = subject, predicate = predicate, object = object),
            .trip_paste, FUN.VALUE = ""
            )
        x <- c(x, ".")
        S7::new_object(
            .parent = x,
            subject = subject,
            predicate = predicate,
            object = object
            )
    }
)

.trip_paste <- function(x) {
    collapse <- if( x[1] %in% c("", "?") ) "" else ":"
    paste0(x, collapse = collapse)
}

#' S7 class to mark domain-specific helper functions.
#' @name term
#' @rdname term-class
#' @description
#' A `term` is an S7 class to reflect an RDF term. In RDF, vocabularies are sets
#' of terms used to describe things. A term is either a class or a property.
#' In practice, `term` is a thin wrapper around `function`.
#' @slot hint `Character scalar`. Used to print nicer information for generated
#'     functions.
#' @param x `function` to be decorated to `term`.
#' @param hint `Character scalar`. Used to print nicer information for generated
#'     functions.
#' @returns an object of class `term`.
#' @examples
#' term
#'
#' @export
#'
term <- S7::new_class(
    "term",
    package = "pallas",
    parent = S7::class_function,
    properties = list(hint = S7::class_character),
    constructor = function(x, hint = "")
        S7::new_object(
            .parent = x,
            hint = hint)
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



