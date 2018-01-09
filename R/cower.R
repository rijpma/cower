cower = function(csv_path, json_path, nquad_path, 
    compress = TRUE, batch_size = -1L){

    schema_list = read_json_schema(json_path)
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

    schema_list = fix_abouturl(schema_list)
    schema_list = prep_table_schema(schema_list = schema_list)
    schema_list = expand_prefixes(schema_list, namespaces)
    schema_list$tableSchema$columns = as.data.frame(schema_list$tableSchema$columns, stringsAsFactors = F)

    done = 0
    current_batch_size = batch_size
    header = data.table::fread(csv_path, header = TRUE, nrow = 1)
    while (current_batch_size == batch_size){
        batch = data.table::fread(csv_path,
            nrows = batch_size, skip = done + 1,
            header = FALSE)
        current_batch_size = nrow(batch)

        setnames(batch, names(batch), names(header))

        convert(dat = batch,
            schema_list = schema_list)

        batch[, graph := named_graphs['assertion']]

        batch = triples(batch, schema_list)

        batch[, pred := colnames_to_predicates(schema_list = schema_list)[as.numeric(pred)]]

        nqwrite(dat = batch[complete.cases(batch), list(sub, pred, obj, graph)],
            nquadpath = nquad_path,
            append = TRUE,
            compress = compress)

        done = done + current_batch_size
        cat("Done ", done, "rows.\n")
    }
}