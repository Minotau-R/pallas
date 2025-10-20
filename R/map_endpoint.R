#' @title Map out a SPARQL endpoint.
#' @name map_endpoint
#' @description
#' Fetch information about known relational information from a sparql endpoint.
#'
#' @inheritParams send_query
#' @export
#' @returns a data frame with ?domain ?property ?range columns.
#' @examples
#' # Don't query endpoints unintentionally.
#' if(FALSE) {
#'     map_endpoint("https://sparql.uniprot.org/")
#' }
#'
map_endpoint <- function(endpoint_url) {
    query <-
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?domain ?property ?range
WHERE {
    ?property rdf:type owl:ObjectProperty .
    ?property rdfs:range ?range .
    ?property rdfs:domain ?domain
}"
    send_query(query, endpoint_url, out_format = "text/csv")
}
