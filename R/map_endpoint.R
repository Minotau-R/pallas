#' @title Map out a SPARQL endpoint.
#' @name map_endpoint
#' @rdname building-queries
#' @description
#' Fetch information about known relational information from a sparql endpoint.
#'
#' @inheritParams send_query
#' @param return.table `Boolean`. Whether to return the raw output table. For
#'     development and debugging. (Default: `FALSE`).
#' @param strip.blank `Boolean`. Whether to remove blank nodes from the result.
#' (Default: `TRUE`)
#' @export
#' @returns a data frame with ?domain ?property ?range columns.
#' @examples
#' # Don't query endpoints unintentionally.
#' if(FALSE) {
#'     x <- map_endpoint(endpoint_url = "https://sparql.uniprot.org/")
#'         x |> where_clause()
#' }
#'
map_endpoint <- function(
        endpoint_url, strip.blank = TRUE, return.table = FALSE
) {
    query <-
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?domain ?range ?property
WHERE {
    ?property rdf:type owl:ObjectProperty .
    ?property rdfs:range ?range .
    ?property rdfs:domain ?domain .
}"
    x <- send_query(
        endpoint_url = endpoint_url, query = query,
        path = "sparql/", out_format = "text/csv"
        )
    stopifnot("Server did not return data. " =  NCOL(x) == 3L)
    if(strip.blank) x <- .not_between_blanks(x)

    if(return.table) {return(x)}
    # Else; (default)
    out_list <- .reponse_to_out_list(x, strip.blank)

    return( out_list )
}

.reponse_to_out_list <- function(x, strip.blank) {
    URIs <- x
    URIs[] <- lapply(x, .keep_URI)
    x[]    <- lapply(x, .drop_URI)
    missed <- x[URIs == x]
    URI_sub <- sub(":.*$", "", missed)
    x_sub   <- sub("^.*:", "", missed)
    vocab <- `[<-`(x, URIs == x, value = x_sub)
    URIs[URIs == x] <- URI_sub

    all_URIs  <- unique(unlist(URIs, use.names = FALSE))

    if(strip.blank) { all_URIs <- all_URIs[all_URIs != "_"] }

    pre_tab <- .make_prefix_table(all_URIs)

    vocab_list <- lapply(
        all_URIs, function(x) unique(vocab[URIs == x])
    )
    names(vocab_list) <- pre_tab[["short"]]

    pred_list <- lapply(
        vocab_list, function(y) (unique(y[y %in% x[["property"]]]))
    )
    pred_list <- pred_list[lengths(pred_list) != 0L]
    pred_list <- mapply(
        predicate_factory,
        x = pred_list, prefix = names(pred_list),
        MoreArgs = list(vocab.table = vocab),
        SIMPLIFY = FALSE
        )

    class_list <- lapply(
        vocab_list,
        function(y) unique( y[y %in% c( vocab[["domain"]], vocab[["range"]] )] )
    )
    class_list <- class_list[lengths(class_list) != 0L]
    class_list <-
        mapply(a_class_factory, class_list, names(class_list), SIMPLIFY = FALSE)
    lexicon <- list(
        P = OWL_env(Reduce(c, pred_list)),
        C = OWL_env(Reduce(c, class_list)),
        V = character(0L),
        prefixes = pre_tab
    )

    OWL(
        .env   = lexicon,
        prefix = .prefix_to_SPARQL(pre_tab)
    )
}

.keep_URI <- function(x) sub("^(.*)/.*", "\\1/", x)
.drop_URI <- function(x) sub("^.*(.*)/", "\\1", x)

#' @description x is a data.frame with three columns. This function filters out
#' rows where the first two columns start with "_:".
#' @noRd
#'
.not_between_blanks <- function(x) {
    `[`(
        x,
        ! do.call(`&`, lapply(x[c(1L, 2L)], grepl, pattern = "^_:") ),

        )
}

.make_prefix_table <- function(uris, prior = NULL) {
    uris <- uris[! uris %in% prior[["uri"]] ]
    short <- sub("^.*(.*)/", "", sub("/$", "", uris))
    bad <- grepl("^[^A-Za-z]", short)
    short[bad] <- sub("^", "x", short[bad])

    prefix_table <- data.frame( short = short, uri = uris )
    rbind.data.frame(prefix_table, prior)
}
