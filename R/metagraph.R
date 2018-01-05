#' @import data.table
metagraph = function(schema_list, csv_path, namespaces){
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

    # metadata in provenance graph
    # jsonld expand does not seem to work...
    pubbnode = bnode()
    tsnode = bnode()
    columnnode = bnode()
    dialectnode = bnode()

    provenance = data.table::rbindlist(
        list(
            provenance,
            list(uriref(schema_list$`@id`, base = ""), uriref("modified", namespaces["dc:"]), literal(schema_list$`dc:modified`$`@value`, schema_list$`dc:modified`$`@type`)),
            list(uriref(schema_list$`@id`, base = ""), uriref("license", namespaces["dc:"]), uriref("", base = schema_list$`dc:license`$`@id`)),
            list(uriref(schema_list$`@id`, base = ""), uriref("title", namespaces["dc:"]), literal(schema_list$`dc:title`, "@en")),
            list(uriref(schema_list$`@id`, base = ""), uriref("publisher", namespaces["dc:"]), pubbnode),
            list(pubbnode, uriref("name", namespaces["schema:"]), literal(schema_list$`dc:publisher`$`schema:name`, "@en")),
            list(pubbnode, uriref("url", namespaces["schema:"]), uriref("", schema_list$`dc:publisher`$`schema:url`$`@id`)),
            list(uriref(schema_list$`@id`, base = ""), uriref("tableSchema", namespaces["csvw:"]), tsnode),
            list(tsnode, uriref("primaryKey", namespaces["csvw:"]), schema_list$`tableSchema`$`primaryKey`),
            list(tsnode, uriref("aboutUrl", namespaces["csvw:"]), schema_list$`tableSchema`$`aboutUrl`),
            list(tsnode, uriref("column", namespaces["csvw:"]), columnnode),
            # columnnode rest/first malarky,
            list(uriref(schema_list$`@id`, base = ""), uriref("dialect", namespaces["csvw:"]), dialectnode),
            list(dialectnode, uriref("quoteChar", namespaces["csvw:"]), schema_list$`dialect`$`quoteChar`), # no datatypes here
            list(dialectnode, uriref("delimiter", namespaces["csvw:"]), schema_list$`dialect`$`delimiter`), # no datatype here
            list(dialectnode, uriref("encoding", namespaces["csvw:"]), schema_list$`dialect`$`encoding`),   # no datatype here
            list(uriref(schema_list$`@id`, base = ""), uriref("url", namespaces["csvw:"]), literal(schema_list$`url`, "xsd:anyURI"))
        )
    )

    # description in provenance graph
    idnodes = bnode(sum(is.na(schema_list$tableSchema$columns[, "@id"])))
    schema_list$tableSchema$columns[!is.na(schema_list$tableSchema$columns[, "@id"]), "@id"] = uriref(schema_list$tableSchema$columns[!is.na(schema_list$tableSchema$columns[, "@id"]), "@id"], base = "")
    schema_list$tableSchema$columns[is.na(schema_list$tableSchema$columns[, "@id"]), "@id"] = idnodes
    clms = vector(mode = "list", length = nrow(schema_list$tableSchema$columns))
    idcol = which(colnames(schema_list$tableSchema$columns) == "@id")
    predicates_from_colnames = stringi::stri_replace_all_regex(
        colnames(schema_list$tableSchema$columns)[-idcol],
        names(namespaces),
        namespaces, vectorize_all = F
    )
    predicates_from_colnames = stringi::stri_join(
        namespaces["csvw:"],
        predicates_from_colnames        
    )

    for (i in 1:nrow(schema_list$tableSchema$columns)){
      clms[[i]] = data.table(
        sub = schema_list$tableSchema$columns[i, idcol], 
        pred = uriref(predicates_from_colnames, base = ""), 
        obj = unlist(schema_list$tableSchema$columns[i, -idcol]))
    }
    clms = data.table::rbindlist(clms)

    clms[grep("Url>$", pred), obj := uriref(rep("", .N), base = obj)]
    clms[pred == "virtual>", obj := literal("true", datatype = "xsd:boolean")]
    clms[!grep("Url>$|^virtual>$", pred), obj := literal(obj, datatype = "xsd:string")]

    provenance = data.table::rbindlist(
        list(
            provenance,
            clms
        )
    )

    # derivations in provenance graph
    provenance = data.table::rbindlist(
        list(
            provenance,
            data.table(uriref(schema_list$tableSchema$columns[, "valueUrl"], base = ""), uriref("wasDerivedFrom", namespaces["prov:"]), literal(schema_list$tableSchema$columns[, "valueUrl"])),
            data.table(uriref(schema_list$tableSchema$columns[, "aboutUrl"], base = ""), uriref("wasDerivedFrom", namespaces["prov:"]), literal(schema_list$tableSchema$columns[, "aboutUrl"])),
            data.table(uriref(schema_list$tableSchema$aboutUrl, base = schema_list$`@context`[[2]]$`@base`), uriref("wasDerivedFrom", namespaces["prov:"]), literal(paste0(schema_list$`@context`[[2]]$`@base`, schema_list$tableSchema$aboutUrl)))
        )
    )

    # rest first stuff in prov graph
    ordlistnodes = bnode(length(schema_list$`tableSchema`$`columns`$`@id`))
    provenance = data.table::rbindlist(
        list(
            provenance,
            data.table(
                ordlistnodes, 
                uriref("first", namespaces["rdf:"]), 
                schema_list$`tableSchema`$`columns`$`@id`
            ),
            data.table(
                ordlistnodes, 
                uriref("rest", namespaces["rdf:"]), 
                c(ordlistnodes[-1], "rdf:nil")
            )
        )
    )

    provenance[, graph := gn['provenance']]

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
