#' @title Send a SPARWL query to an endpoint.
#' @name send_query
#' @description
#' Query a SPARQL endpoint with SPARQL.
#' @param query `Character scalar` SPARQL query content.
#' @param out_format `Character scalar`. (Default: `"text/csv"`.)
#' @param endpoint_url `Character scalar` sparql endpoint url.
#'
#' @export
#'
send_query <- function(query, endpoint_url, out_format = "text/csv") {
    stopifnot(
        "Arg 'endpoint_url' must be a valid URL." = .is_valid_URL(endpoint_url)
    )
    query <- .build_SPARQL(query, endpoint_url)
    .query_SPARQL(query, out_format)
}

#' @importFrom utils URLencode
#' @noRd
#'
.build_SPARQL <- function(query, endpoint_url) {
    query <- gsub("\\+", "%2B", utils::URLencode(query, reserved = TRUE))
    query <- paste0(endpoint_url, "?query=", query)
    return(query)
}

#' @importFrom httr content GET timeout add_headers
#' @importFrom utils read.csv
#' @param query `Character scalar`. Full query url.
#' @noRd
#'
.query_SPARQL <- function(query, out_format = "text/csv") {

    result <- httr::content(
        httr::GET(
            query,
            httr::add_headers(c(Accept = out_format))
        ),
        "text", encoding = "UTF-8"
    )

    utils::read.csv(textConnection(result), stringsAsFactors = TRUE)
}


#' @importFrom httr parse_url
#' @description
#' Does url have a non-null scheme AND hostname?
#' @noRd
#'
.is_valid_URL <- function(x) {
    !any(
        vapply(httr::parse_url(x)[c("scheme", "hostname")], is.null, FALSE)
    )
}

