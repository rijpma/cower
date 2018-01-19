# helper functions go here

urn = function(n = 1){
    replicate(n, uriref(uuid::UUIDgenerate(), base = "uuid:urn:"))
}

rdf_regex = function(){
    # https://www.w3.org/2001/sw/DataAccess/proto-tests/tools/ntriples.py

    return(
        list(
            uriref = '<([^:]+:[^\\s"<>]+)>',
            literal = '"([^"\\\\]*(?:\\\\.[^"\\\\]*)*)"',
            litinfo = paste0('(?:@([a-z]+(?:-[a-z0-9]+)*)|\\^\\^', urirefrgx, ')?',
            nodeid = '_:([A-Za-z][A-Za-z0-9]*)')
        )
    )
}

clean_string = function(string){
    stringi::stri_replace_all_fixed(
        str = string, 
        pattern     = c('\\',   '"',   '\r\n', '\n'), # order is important, later replace previous replacements
        replacement = c('\\\\', '\\"', ' ',    ' '),  # alt: \r\n -> \\r\\n but line breaks have no place in strings yes?
        vectorize_all = F
    )
}
