#' Convert a csv file to nquads using json-ld data description.
#' @param csv_path The path to the csv file.
#' @param json_path The path to the json-ld data description. 
#' Use \code{\link{build_schema_list}} and \code{\link{schema_json}} to make one or use COW.
#' @param nquad_path Destination path for nquads. \code{cower} will
#' give a warning if the extension does not match the \code{compress} option.
#' @param compress. Should cower output be a compressed (gz) file.
#' Defaults to \code{TRUE} as nquad files can be very large relative
#' to the original file (x10, roughly).
#' @param batch_size Batch size for conversion. Default is \code{-1L},
#' all rows at once. For large csv-files that take up considerable memory
#' to convert, setting this to a lower number will preserve memory.
#' @param max_size Set this parameter to an integer to only convert 
#' part of the dataset. Useful for testing.
#' @export
cower = function(csv_path, json_path, nquad_path, 
    compress = TRUE, batch_size = -1L, max_size = FALSE){

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

    if (max_size) csv_path = paste0("head -", format(max_size, scientific = FALSE), " ", csv_path)
    done = 0
    current_batch_size = batch_size
    header = data.table::fread(csv_path, header = TRUE, nrow = 1)
    while (current_batch_size == batch_size){
        batch = data.table::fread(csv_path,
            nrows = batch_size, skip = done + 1,
            header = FALSE,
            sep = schema_list$dialect$delimiter)
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