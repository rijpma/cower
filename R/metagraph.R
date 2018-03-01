metagraph = function(schema_list, graph_names){

    nquads = jsonld::jsonld_to_rdf(jsonlite::toJSON(schema_list, auto_unbox = TRUE))

    # expand blanknodes
    blanknodes = unique(
        unlist(
            stringi::stri_extract_all_regex(nquads, "_:b[0-9]+")
        )
    )
    nquads = stringi::stri_replace_all_regex(nquads, blanknodes, bnode(length(blanknodes)), vectorize_all = F)

    # lines into table for safer replacing
    nquads = stringi::stri_split_fixed(nquads, '\n', simplify = T)
    nquadtable = as.data.table(tstrsplit(nquads, '[ \t]'))
    nquadtable[is.na(nquadtable)] <- ""
    nquadtable = nquadtable[, list(sub = V1, pred = V2, obj = do.call(paste0, .SD)), .SDcols = -c(1:2)]
    nquadtable[, obj := stringi::stri_replace_last_fixed(obj, ".", "")]
    nquadtable[nquadtable == ""] = NA

    # replace curly brackets from urirefs, should only be necessary on obj 
    nquadtable[stringi::stri_detect_regex(sub, "^<.*>$"), sub := stringi::stri_replace_all_regex(sub, "[{}]", "_")]
    nquadtable[stringi::stri_detect_regex(pred, "^<.*>$"), pred := stringi::stri_replace_all_regex(pred, "[{}]", "_")]
    nquadtable[stringi::stri_detect_regex(obj, "^<.*>$"), obj := stringi::stri_replace_all_regex(obj, "[{}]", "_")]

    # add graph
    nquadtable[, graph := graph_names["provenance"]]

    return(nquadtable)
}

#' @import data.table
nanopublication = function(schema_list, graph_names, namespaces, hashes, metadatagraph){

    # needs https://github.com/CLARIAH/COW/blob/master/cow/converter/csvw.py

    now = format(Sys.time(), "%Y-%m-%dT%H:%M")

    bgraph = urn(n = 1)
    
    creators = metadatagraph[pred == "<http://purl.org/dc/terms/creator>", obj]
    if (length(creators) == 0) creators = NA

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
            ),
            data.table(
                sub = graph_names['nanopublication'], 
                pred = uriref('wasAttributedTo', base = namespaces['prov:']), 
                obj = creators
            )
        )
    )

    overall[, graph := bgraph]

    # provenance graph
    ### Provenance information for the assertion graph (the data structure definition itself)

    # about/property/valueUrl derivations also included 
    tsurls = c(schema_list$tableSchema$columns[["aboutUrl"]],
               schema_list$tableSchema$columns[["propertyUrl"]],
               schema_list$tableSchema$columns[["valueUrl"]])
    tsurls  = na.omit(unique(tsurls))
    escaped = urltools::url_decode(uriref(tsurls, base = ""))
    not_escaped = urltools::url_decode(uriref(rep("", length = length(tsurls)), base = tsurls))

    provenance = data.table::rbindlist(
        list(
            list(graph_names['assertion'], uriref("wasDerivedFrom", namespaces["prov:"]), metadatagraph[pred == "<http://www.w3.org/ns/csvw#url>", sub]),
            list(graph_names['assertion'], uriref("wasDerivedFrom", namespaces["prov:"]), uriref(hashes['full'], namespaces['sdr:'])),
            list(graph_names['assertion'], uriref("generatedAtTime", namespaces["prov:"]), literal(now, datatype = uriref('dateTime', namespaces['xsd:']))),
            data.table(
                graph_names['assertion'], 
                uriref('wasAttributedTo', base = namespaces['prov:']),
                creators
            ),
            data.table(
                escaped[escaped != not_escaped], 
                uriref("wasDerivedFrom", base = namespaces['prov:']),
                literal(tsurls[escaped != not_escaped], datatype = uriref("string", base = namespaces['xsd:']))
            )
            # data.table( # should be redundant but keep for now
            #     graph_names['assertion'],
            #     uriref("creator", base = namespaces['dc:']),
            #     uriref("", base = creators)
            # )
        )
    )

    provenance[, graph := graph_names['provenance']]

    # pubinfo graph
    pubinfo = data.table::rbindlist(
        list(
            list(graph_names['nanopublication'], uriref("wasGeneratedBy", namespaces["prov:"]), uriref("", base = "http://github.com/rijpma/cower")),
            list(graph_names['nanopublication'], uriref("generatedAtTime", namespaces["prov:"]), literal(now, datatype = uriref('dateTime', namespaces['xsd:']))),
            data.table(
                graph_names['assertion'], 
                uriref("creator", base = namespaces['dc:']), 
                creators
            )
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

    return(out[complete.cases(out)])
}
