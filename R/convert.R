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
literal = function(string, datatype = "xsd:string", ...){
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
uriref = function(string, base, path = '', ...){
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
convert = function(df, schema_list, 
    type = "", datatype = ""){

    table_schema = schema_list$tableSchema$columns
    table_schema = table_schema[order(table_schema$virtual), ]

    # type = match.arg(type)
    # check df = character

    x = rbind(table_schema[, c("titles", "type", "datatype", "aboutUrl_base", "aboutUrl_eval")],
          setNames(table_schema[, c("titles", "type", "datatype", "valueUrl_base", "valueUrl_eval")],
            c("titles", "type", "datatype", "aboutUrl_base", "aboutUrl_eval")))
    x$type[1:nrow(table_schema)] = "uriref"
    x$titles[1:nrow(table_schema)] = paste0(x$titles[1:nrow(table_schema)], "_sub")

    string_to_eval = paste0(df, "[, ", x$titles, 
        ":= ", x$type, "(", 
        x$aboutUrl_eval, ", ",
        "base = '", x$aboutUrl_base, "', ",
        "datatype = '", x$datatype,
        "')]")

    eval(parse(text = string_to_eval), envir = parent.frame(2))
}

subjects = function(df_long, schema_list){

    # check if ncol(df_long == 2)

    table_schema = schema_list$tableSchema$columns

    x = eval(parse(text = paste0(
        df_long, "[, list(N = .N), by = list(pred = pred)]"
        )),
        envir = parent.frame(2))

    N = x$N
    predicates = x$pred
    # only predicates needed

    string_to_eval = paste0(
        "uriref(", df_long, "[pred == '",
            predicates, "', ",
            table_schema$aboutUrl_eval,
            "], base = '", schema_list$`@context`[[2]]$`@base`, "')")
    # fix base in schema prep, getting messy
    string_to_eval = paste0(string_to_eval, collapse = ", ")
    string_to_eval = paste0("c(", string_to_eval, ")")
    eval(parse(text = string_to_eval),
        envir = parent.frame(2))
}

colnames_to_predicates = function(schema_list){
    ifelse(is.na(schema_list$tableSchema$columns$propertyUrl),
        uriref(schema_list$tableSchema$columns$propertyUrl_eval, base = schema_list$`@context`[[2]]$`@base`),
        paste0("<", schema_list$tableSchema$columns$propertyUrl, ">"))
}