#' Create literals
#' 
#' \code{literal} creates a literal from a value with escaped quotes for file writing and appends a datatype.
#' 
#' @param string The value to be converted into a literal.
#' @param datatype The datatype of the literal. Defaults to xsd:string. Language tags are identified by \code{@}.
#' @return A literal with escaped quotes and the datatype appended. If \code{string} is \code{NA}, \code{NA} is returned.
#' @examples
#' literal("example", datatype="xsd:string")
#' literal("example", datatype="@@en")
#' literal(630.6, datatype="xsd:float")
#' literal(NA, datatype="xsd:integer")
literal = function(string, datatype = "xsd:string"){
    if(grepl("^@", datatype)){
        ifelse(is.na(string), NA, paste0('"', string, '"', datatype))
    } else {
        ifelse(is.na(string), NA, paste0('"', string, '"^^', datatype))
    }
}

# todo: check for missing / or # and insert, maybe based on default
#' Create URI
#' 
#' \code{uriref} creates a uri with nquad-angle brackets and any non-
#' safe characters are %-encoded or replaced by underscores.
#' 
#' @param string The value to be converted to a URI.
#' @param base The base of URI.
#' @param path Further path of URI
#' @examples
#' uriref('rainy', base = 'http://www.weather.com/', path = 'weather/')
uriref = function(string, base, path = ''){
    to_replace = "[ %+&]"

    ifelse(is.na(string), NA, 
    paste0("<", base, path, urltools::url_encode(gsub(to_replace, "_", string)), ">"))
}

#' Create blank node
bnode = function(n = 1){
    replicate(n, paste0("_:N", gsub("-", "", uuid::UUIDgenerate())))
     # N in nq serialisation to make NCName compliant-ish
}

# todo: have uris and triples in one go? have multiple bases possible and have one = recyle
# merge paths into base?
# make paths optional 
# make it possible to combine
# support column names and types into named list?
# check for character-type df?
# support non-character df?
# default column names = all

#' Efficiently convert selected columns of \code{data.table} to 
#' nquad statements
#' 
#' @param df Name of the data.table as a length one character vector.
#' @param column_names Names of the data.table to be converted as a character vector.
#' @param base Base of the URIs to be minted in \code{uriref},
#' @param paths Further path of URI to be minted in \code{uriref}.
#' @param type Type of nquads statements: \code{'uri'} or \code{'literal'}.
#' @param datatypes Character vector containing the datatype of literals. May include language statements.
#' @examples
#' the_data = data.table(refs = c('a', 'b', 'c'), ltrl = c(1, 2, 3))
#' convert(df = "the_data", column_names = "ltrl", type = 'literal', datatypes = 'xsd:int')
#' convert(df = "the_data", column_names = "refs", type = 'uri', base = 'https://www.example.org/', paths = 'data/')
#' print(the_data)
convert = function(df, column_names, base, paths,
    type = c("uri", "literal"), datatypes = "xsd:string"){

    type = match.arg(type)
    # check df = character

    if (type == "uri"){
        string_to_eval = paste0(df, "[, ", column_names, ":= uriref(", 
            column_names, ", base = '", base, "', path = '", paths, "')]")
     } else if (type == "literal"){
        string_to_eval = paste0(df, "[, ", column_names, ":= literal(", 
            column_names, ", datatype = '", datatypes, "')]")
    }

    eval(parse(text = string_to_eval), envir = parent.frame(2))
}