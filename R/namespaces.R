namespaces = function(namespace){
    out = c(
        rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        skos = "http://www.w3.org/2004/02/skos/core#",
        xsd = "http://www.w3.org/2001/XMLSchema#",
        sdmxdim = "http://purl.org/linked-data/sdmx/2009/dimension#"
    )
    return(out[namespace])
}
