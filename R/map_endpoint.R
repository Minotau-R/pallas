#' @title Map out a SPARQL endpoint.
#' @name map_endpoint
#' @description
#' Fetch information about known relational information from a sparql endpoint.
#'
#' @inheritParams send_query
#' @param return.table `Boolean`. Whether to return the raw output table. For
#'     development and debugging. (Default: `FALSE`).
#' @export
#' @returns a data frame with ?domain ?property ?range columns.
#' @examples
#' # Don't query endpoints unintentionally.
#' if(FALSE) {
#'     x <- map_endpoint("https://sparql.uniprot.org/")
#' }
#'
map_endpoint <- function(endpoint_url, return.table = FALSE) {
    query <-
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?domain ?range ?property
WHERE {
    ?property rdf:type owl:ObjectProperty .
    ?property rdfs:range ?range .
    ?property rdfs:domain ?domain
}"
    x <- send_query(query, endpoint_url, out_format = "text/csv")

    if(return.table) {return(x)}
    # Else; (default)

    URIs <- x
    URIs[] <- lapply(x, .keep_URI)
    x[]    <- lapply(x, .drop_URI)
    missed <- x[URIs == x]
    URI_sub <- sub(":.*$", "", missed)
    x_sub   <- sub("^.*:", "", missed)
    vocab <- `[<-`(x, URIs == x, value = x_sub)
    URIs[URIs == x] <- URI_sub

    all_URIs  <- unique(unlist(URIs, use.names = FALSE))

    vocab_list <- lapply(
        all_URIs,
        function(x) unique(vocab[URIs == x])
    )

    names(vocab_list) <- all_URIs

    pred_list <- lapply(
        vocab_list,
        function(y) (unique(y[y %in% x$property]))
    )
    pred_list <- pred_list[lengths(pred_list) != 0L]
    pred_list <- lapply(pred_list, predicate_factory, vocab.table = vocab)

    class_list <- lapply(
        vocab_list,
        function(y) (unique(y[y %in% c(vocab$domain, vocab$range)]))
    )
    class_list <- class_list[lengths(class_list) != 0L]

    out_list <- list(
        P = pred_list,
        C = class_list
    )

    return( out_list )
}

.keep_URI <- function(x) sub("^(.*)/.*", "\\1", x)
.drop_URI <- function(x) sub("^.*(.*)/", "\\1", x)
