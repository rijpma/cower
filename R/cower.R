#' Convert a csv file to nquads using json-ld data description.
#' @param csv_path The path to the csv file.
#' @param json_path The path to the json-ld data description. 
#' Use \code{\link{build_schema_list}} and \code{\link{schema_json}} to make one or use COW.
#' @param nquad_path Destination path for nquads. \code{cower} will
#' give a warning if the extension does not match the \code{compress} option.
#' @param compress. Should cower output be a compressed (gz) file?
#' Defaults to \code{TRUE} as nquad files can be very large relative
#' to the original file (x10, roughly).
#' @param batch_size Batch size for conversion. Default is \code{-1L},
#' all rows at once. For large csv-files that take up considerable memory
#' to convert, setting this to a lower number will preserve memory.
#' @param max_size Set this parameter to an integer to only convert 
#' part of the dataset. Useful for testing. Depends on \code{head} command line tool.
#' @export
cower = function(csv_path, json_path, nquad_path, 
    compress = TRUE, batch_size = -1L, max_size = FALSE){

    schema_list = read_json_schema(json_path)

    namespaces = get_namespaces(schema_list) # get_namespaces does not function on expanded schema_list
    filehash = hash_file(csv_path)

    named_graphs = graph_names(filehash['short'], base = schema_list$`@context`[[2]]$`@base`)

    # mg is expanded json-ld and needs the original json
    # or why not just the json path and make life easier?
    mg = metagraph(schema_list, graph_names = named_graphs)
    
    schema_list = fix_abouturl(schema_list)
    schema_list = prep_table_schema(schema_list = schema_list)
    # if you want lists in the xUrls you should probably double the rows at this stage because expand_prefixes() only works on prefixes at start
    schema_list = expand_prefixes(schema_list, namespaces)
    schema_list$tableSchema$columns = as.data.frame(schema_list$tableSchema$columns, stringsAsFactors = F)
    schema_list = datatypes_as_urirefs(schema_list)

    # nanopublication needs the exanded schema_list
    # maybe add check for == expanded
    nanopub = nanopublication(schema_list = schema_list, 
        graph_names = named_graphs,
        namespaces = namespaces,
        hashes = filehash,
        metadatagraph = mg)

    nqwrite(dat = rbindlist(list(nanopub, mg)),
        nquadpath = nquad_path, 
        append = FALSE,
        compress = compress)


    if (max_size) csv_path = paste0("head -", format(max_size, scientific = FALSE), " ", csv_path)
    # if (compressed_csv) csv_path = paste0("gunzip -c ", csv_path)
    # if (compressed_csv & max_size) csv_path = paste0("gunzip -c ", csv_path, " | head - ", format(max_size, scientific = FALSE))
        # but this probably won't play nice with `done`
    done = 0
    current_batch_size = batch_size
    header = data.table::fread(csv_path, header = TRUE, nrow = 1)
    while (current_batch_size == batch_size){

        getout = FALSE
        
        tryCatch(
            batch <- data.table::fread(csv_path,
                nrows = batch_size, 
                skip = done + 1,
                header = FALSE,
                sep = schema_list$dialect$delimiter),
            error = function(x) getout <<- TRUE
        )
        
        if (getout) break

        current_batch_size = nrow(batch)

        data.table::setnames(batch, names(batch), names(header))

        convert(dat = batch,
            schema_list = schema_list,
            done_so_far = done)

        batch[, graph := named_graphs['assertion']]

        batch = triples(batch, schema_list)

        batch[, pred := colnames_to_predicates(schema_list = schema_list)[as.numeric(pred)]]

        nqwrite(dat = batch[complete.cases(batch), list(sub, pred, obj, graph)],
            nquadpath = nquad_path,
            append = TRUE,
            compress = compress)

        done = done + current_batch_size
        cat("Converted", done, "rows.\n")
    }
    cat("Done.")
}
