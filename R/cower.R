cower = function(csv_path, json_path, nquad_path, 
    compress = TRUE){

    schema_list = read_json_schema(json_path)
    dat = data.table::fread(csv_path)
    namespaces = get_namespaces(schema_list) # get_namespaces does not function on expanded schema_list
    filehash = githash(csv_path)

    named_graphs = graph_names(filehash['short'], base = schema_list$`@context`[[2]]$`@base`)

    nanopub = nanopublication(schema_list = schema_list, 
        graph_names = named_graphs,
        namespaces = namespaces,
        hashes = filehash)

    nqwrite(dat = nanopub, 
        nquadpath = nquad_path, 
        append = FALSE,
        compress = compress)

    mg = metagraph(schema_list, named_graphs)

    if (compress) {
        outfile = gzfile(nquad_path, open = "a")
    } else {
        outfile = file(nquad_path, open = "a")
    }
    writeLines(text = mg, con = outfile)
    close(outfile)

    schema_list = prep_table_schema(schema_list = schema_list)
    schema_list = expand_prefixes(schema_list, namespaces)
    schema_list$tableSchema$columns = as.data.frame(schema_list$tableSchema$columns, stringsAsFactors = F)

    convert(dat = dat, 
        schema_list = schema_list)
    
    dat[, graph := graph_names(csv_path, base = schema_list$`@context`[[2]]$`@base`)["assertion"]]
    
    dat = triples(dat, schema_list)
    
    dat[, pred := colnames_to_predicates(schema_list = schema_list)[as.numeric(pred)]]

    nqwrite(dat = dat[complete.cases(dat), list(sub, pred, obj, graph)], 
        nquadpath = nquad_path, 
        append = TRUE,
        compress = compress)
}