# helper functions go here

urn = function(n = 1){
    replicate(n, uriref(uuid::UUIDgenerate(), base = "uuid:urn:"))
}

rdf_regex = function(type = c("uriref", "literal", "nodid", "litinfo")){
    # https://www.w3.org/2001/sw/DataAccess/proto-tests/tools/ntriples.py

    out =  c(
            uriref = '<([^:]+:[^\\s"<>]+)>',
            literal = '"([^"\\\\]*(?:\\\\.[^"\\\\]*)*)"',
            nodeid = '_:([A-Za-z][A-Za-z0-9]*)'
    )
    out["litinfo"] = paste0('(?:@([a-z]+(?:-[a-z0-9]+)*)|\\^\\^', out["uriref"], ')?')
    return(out[type])
}

clean_string = function(string){
    stringi::stri_replace_all_fixed(
        str = string, 
        pattern     = c('\\',   '"',   '\r\n', '\n'), # order is important, later replace previous replacements
        replacement = c('\\\\', '\\"', ' ',    ' '),  # alt: \r\n -> \\r\\n but line breaks have no place in strings yes?
        vectorize_all = F
    )
}

countlines = function(csv_path){
    n = nrow(data.table::fread(csv_path, select = 1L))
    # n = system2(command = "wc", args = c('-l', csv_path, " | awk '{print $1}'"), stdout = T)
    # n = system2(command = "cat", args = c(csv_path, " | wc -l"), stdout = T)
    # would need windows alternative `find /c /v ""`
    return(as.numeric(n))
}

read_rdf = function(path_to_rdf, type = c("ntriples", "nquads")){
    dat = readLines(path_to_rdf)
    dat = stringi::stri_split_fixed(dat, "\n", simplify = TRUE)
    dat = tstrsplit(dat, '[ \t]')
    dat = as.data.table(dat)
    dat[is.na(dat)] = "" # for safe pasting

    dat = dat[, list(sub = V1, pred = V2, obj = do.call(paste0, .SD)), .SDcols = -c(1:2)]
    dat[, obj := stringi::stri_replace_last_fixed(obj, ".", "")]

    if (type == "nquads"){
        dat[, graph := sapply(stringi::stri_extract_last_regex(obj, rdf_regex("uriref")), tail, 1)]
        dat[, object := stringi::stri_replace_last_fixed(obj, graph, "")]
    }
    return(dat[])
}
