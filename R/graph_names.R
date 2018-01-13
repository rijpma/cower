#' Creates URIs for graphs
#' 
#' \code{graph_names} creates URIs for graphs based on a file
#' from file hash and system time.
#'
#' @param path_to_csv The path to the csv file.
#' @param base The base URI to be used in the rdf.
#' @param sub_path Further path of the URI.
#' @return A named vector with five graph names for the 
#'      assertion, nanopublication, provenance, pubinfo, and
#'      resource graphs.
#' @examples
#' path = tempfile()
#' outfile = file(path, "wb")
#' write.csv(data.frame(a = c(1, 2, 3)), file = outfile)
#' close(outfile)
#' graph_names(path)
graph_names = function(filehash, 
    base = ""){

    now = format(Sys.time(), "%Y-%m-%dT%H:%M")
    graphs = c('assertion', 'nanopublication', 'provenance', 'pubinfo', 'resource')

    # better: use uriref
    nanopub = paste0("<", base, graphs, "/", 
         filehash, "/", now, ">")

    names(nanopub) = graphs

    return(nanopub)
}

hash_file = function(path_to_csv){
    filehash = digest::digest(file = path_to_csv, algo = "md5")
    return(c(full = filehash, short = substring(filehash, 1, 8)))
}
