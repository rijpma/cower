metagraph = function(schema_list){
    # overall, pubinfo, nanopublication graphs
    # and then
    nquads = jsonld::jsonld_to_rdf(toJSON(schema_list, auto_unbox = TRUE))
    blanknodes = unique(unlist(stringi::stri_extract_all_regex(nquads, "_:b[0-9]+")))
    nquads = stringi::stri_replace_all_regex(nquads, blanknodes, bnode(length(blanknodes)), vectorize_all = F)
    nquads = stringi::stri_replace_all_regex(nquads, ".\n", "graphnamegoeshere .\n")

    return(nquads)
}

#' @import data.table
nanopublication = function(schema_list, csv_path, namespaces){
    gn = graph_names(csv_path, base = schema_list$`@context`[[2]]$`@base`)
    now = format(Sys.time(), "%Y-%m-%dT%H:%M")
    hashes = githash(csv_path)

    bgraph = urn(n = 1)
    
    overall = data.table::rbindlist(
        list(
            data.table(
                sub = uriref(hashes['full'], namespaces['sdr:']), 
                pred = c(uriref("path", namespaces['sdv:']), 
                    uriref("sha1_hash", namespaces['sdv:'])), 
                obj = c(literal(path.expand("~/repos/cower/test.csv"), "xsd:string"),
                    literal(hashes['full'], "xsd:string"))
            ),
            ## nanopub
            ### the nanopublication
            data.table(
                sub = gn['nanopublication'] ,
                pred = c(uriref("type", namespaces['rdf:']), uriref("hashasAssertion", namespaces["np:"])),
                obj = c(uriref("Nanopublication", namespaces["np:"]), gn['assertion'])
            ),
            data.table(
                sub = gn['assertion'],
                pred = c(uriref("type", namespaces["rdf:"]), uriref("hasProvenance", namespaces["np:"])),
                obj = c(uriref("assertion", namespaces["np:"]), gn['provenance'])
            ),
            data.table(
                sub = gn['provenance'],
                pred = uriref("type", namespaces["rdf:"]),
                obj = uriref("Provenance", namespaces["np:"])
            ),
            # link to publication info graph
            data.table(
                sub = gn['assertion'],
                pred = uriref("hasPublicationInfo", namespaces["np:"]),
                obj = gn['pubinfo']
            ),
            data.table(
                sub = gn['pubinfo'],
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
            list(gn['assertion'], uriref("wasDerivedFrom", namespaces["prov:"]), uriref(hashes['full'], namespaces['sdr:'])),
            list(gn['assertion'], uriref("generatedAtTime", namespaces["prov:"]), literal(now, datatype = 'xsd:dateTime'))
        )
    )

    # pubinfo graph
    pubinfo = data.table::rbindlist(
        list(
            list(gn['nanopublication'], uriref("wasGeneratedBy", namespaces["prov:"]), uriref("", base = "http://github.com/rijpma/cower")),
            list(gn['nanopublication'], uriref("generatedAtTime", namespaces["prov:"]), literal(now, datatype = 'xsd:dateTime'))
        )
    )
    pubinfo[, graph := gn['pubinfo']]

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
