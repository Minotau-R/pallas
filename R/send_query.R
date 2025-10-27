#' @title Send a SPARWL query to an endpoint.
#' @name send_query
#' @description
#' Query a SPARQL endpoint with SPARQL.
#' @param endpoint_url `Character scalar` sparql endpoint url.
#' @param query `Character scalar` SPARQL query content.
#' @param path `Character scalar` Optional. Path from endpoint to query.
#'     (Default: `'sparql/'`)
#' @param out_format `Character scalar`. (Default: `"text/csv"`.)
#'
#' @export
#'
send_query <- function(
        endpoint_url, query, path = "sparql/", out_format = "text/csv"
        ) {
    stopifnot(
        "Arg 'endpoint_url' must be a valid URL." = .is_valid_URL(endpoint_url)
    )
    .query_SPARQL(
        url = endpoint_url, path = path, query = query, out_format = out_format
        )
}

#' @importFrom utils URLencode
#' @noRd
#'
.SPARQLencode <- function(query) {
    query <- gsub("\\+", "%2B", utils::URLencode(query, reserved = TRUE))
    return(query)
}

#' @importFrom httr content GET accept
#' @importFrom utils read.csv
#' @param query `Character scalar`. Full query url.
#' @noRd
#'
.query_SPARQL <- function(url, path, query, out_format = "text/csv") {
    query <- .SPARQLencode(query)

    url <- httr::parse_url(url)
    url$path <- path
    url$query <- list(query = I(query))
    url <- httr::build_url(url)


    result <- httr::content(
        httr::GET(
            url,
            httr::accept(out_format)
            ), as = "text", encoding = "UTF-8"
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


# Adapted from UniProt.ws utilities.R
#' @importFrom BiocFileCache BiocFileCache bfcneedsupdate bfcrpath bfcdownload
#' @importFrom tools R_user_dir
#' @importFrom rlang hash
.getCache <- function(url) {

    cache <- tools::R_user_dir("pallas", "cache")

    bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)

    # url_hash <- rlang::hash(url)

    rpath <- BiocFileCache::bfcrpath(
        bfc, rnames = url, exact = TRUE, download = TRUE, rtype = "web"
    )

    to.update <- BiocFileCache::bfcneedsupdate(bfc, names(rpath))

    if( to.update ){
        BiocFileCache::bfcdownload(bfc, names(rpath), ask = FALSE)
    }

    return(rpath)
}
