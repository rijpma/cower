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
