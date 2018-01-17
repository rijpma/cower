metagraph = function(schema_list, graph_names){
    # overall, pubinfo, nanopublication graphs
    # and then
    nquads = jsonld::jsonld_to_rdf(jsonlite::toJSON(schema_list, auto_unbox = TRUE))
    blanknodes = unique(
        unlist(
            stringi::stri_extract_all_regex(nquads, "_:b[0-9]+")
        )
    )
    nquads = stringi::stri_replace_all_regex(nquads, blanknodes, bnode(length(blanknodes)), vectorize_all = F)
    nquads = stringi::stri_replace_all_regex(nquads, ".\n", paste0(graph_names["provenance"], " .\n"))

    return(nquads)
}

#' @import data.table
nanopublication = function(schema_list, graph_names, namespaces, hashes){
    now = format(Sys.time(), "%Y-%m-%dT%H:%M")

    bgraph = urn(n = 1)
    
    overall = data.table::rbindlist(
        list(
            data.table(
                sub = uriref(hashes['full'], namespaces['sdr:']), 
                pred = c(uriref("path", namespaces['sdv:']), 
                    uriref("md5_hash", namespaces['sdv:'])), 
                obj = c(literal(path.expand("~/repos/cower/test.csv"), uriref("string", namespaces["xsd:"])),
                    literal(hashes['full'], uriref("string", namespaces["xsd:"])))
            ),
            ## nanopub
            ### the nanopublication
            data.table(
                sub = graph_names['nanopublication'] ,
                pred = c(uriref("type", namespaces['rdf:']), uriref("hashasAssertion", namespaces["np:"])),
                obj = c(uriref("Nanopublication", namespaces["np:"]), graph_names['assertion'])
            ),
            data.table(
                sub = graph_names['assertion'],
                pred = c(uriref("type", namespaces["rdf:"]), uriref("hasProvenance", namespaces["np:"])),
                obj = c(uriref("assertion", namespaces["np:"]), graph_names['provenance'])
            ),
            data.table(
                sub = graph_names['provenance'],
                pred = uriref("type", namespaces["rdf:"]),
                obj = uriref("Provenance", namespaces["np:"])
            ),
            # link to publication info graph
            data.table(
                sub = graph_names['assertion'],
                pred = uriref("hasPublicationInfo", namespaces["np:"]),
                obj = graph_names['pubinfo']
            ),
            data.table(
                sub = graph_names['pubinfo'],
                pred = uriref("type", namespaces["rdf:"]),
                obj = uriref("PublicationInfo", namespaces["np:"])
            )
        )
    )

    overall[, graph := bgraph]

    # provenance graph
    ### Provenance information for the assertion graph (the data structure definition itself)
    provenance = data.table::rbindlist(
        list(
            list(graph_names['assertion'], uriref("wasDerivedFrom", namespaces["prov:"]), uriref(hashes['full'], namespaces['sdr:'])),
            list(graph_names['assertion'], uriref("generatedAtTime", namespaces["prov:"]), literal(now, datatype = uriref('dateTime', namespaces['xsd:'])))
        )
    )

    provenance[, graph := graph_names['provenance']]

    # pubinfo graph
    pubinfo = data.table::rbindlist(
        list(
            list(graph_names['nanopublication'], uriref("wasGeneratedBy", namespaces["prov:"]), uriref("", base = "http://github.com/rijpma/cower")),
            list(graph_names['nanopublication'], uriref("generatedAtTime", namespaces["prov:"]), literal(now, datatype = uriref('dateTime', namespaces['xsd:'])))
        )
    )
    pubinfo[, graph := graph_names['pubinfo']]

    out = data.table::rbindlist(
        list(
            provenance,
            pubinfo,
            overall
        )
    )
    setnames(out, names(out), c("sub", "pred", "obj", "graph"))

    return(out)
}
