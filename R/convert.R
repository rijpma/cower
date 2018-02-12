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
literal = function(string, datatype = "xsd:string", lang = NA, ...){
    # prettier way of doing this? Get datatype and connector based on content datatype?
    if (!is.na(lang)){
        ifelse(
            is.na(string),
            NA,
            paste0('"', clean_string(string), '"@', lang)
        )
    } else if (is.na(datatype)){
        ifelse(
            is.na(string),
            NA,
            paste0('"', clean_string(string), '"')
        )
    } else {
        ifelse(
            is.na(string),
            NA,
            paste0('"', clean_string(string), '"^^', datatype)
        )
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
# merge paths into base?
# make paths optional 
# support column names and types into named list?

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
convert = function(dat, schema_list, 
    type = "", datatype = "", done_so_far = 0){

    dat = deparse(substitute(dat))

    table_schema = as.data.frame(schema_list$tableSchema$columns, stringsAsFactors = FALSE)
    table_schema = table_schema[order(table_schema$virtual, decreasing = TRUE), ]
    # virtual is character here, but order still works

    x = rbind(table_schema[, c("titles", "column", "type", "datatype", "aboutUrl_base", "aboutUrl_eval", "null")],
          setNames(table_schema[, c("titles", "column", "type", "datatype", "valueUrl_base", "valueUrl_eval", "null")],
            c("titles", "column", "type", "datatype", "aboutUrl_base", "aboutUrl_eval", "null")))
    # fails when null not present
    x$type[1:nrow(table_schema)] = "uriref"
    x$newvar = x$titles
    x$newvar[1:nrow(table_schema)] = paste0(x$newvar[1:nrow(table_schema)], "_sub")
    # x$titles[1:nrow(table_schema)] = paste0(x$titles[1:nrow(table_schema)], "_sub")

    x$aboutUrl_eval = stringi::stri_replace_all_fixed(x$aboutUrl_eval, ".I", stringi::stri_join("(.I + " , + done_so_far, ")"))

  
    convertstring = paste0(
        "`", x$newvar, "`",
        " := ", 
        x$type, "(", 
            "string = ", x$aboutUrl_eval, ", ",
            "base = '", x$aboutUrl_base, "', ",
            "datatype = '", x$datatype, # this quote needs to be fixed if you want to pass NA as NA
        "')"
    )
    
    # but for now this ugly solution
    convertstring = stringi::stri_replace_all_fixed(convertstring, "'NA'", "NA")

    nullstring = ifelse(x$null == "NULL", 
        "",
        paste0("!", x$column, " %in% ", x$null)
    )


    string_to_eval = paste0(
        dat, "[",
            nullstring,
            ", ",
            convertstring,
        "]"
    )
    # save time and risky %chin% behaviour by dropping if NULL?


    # string_to_eval = paste0(dat, "[, `", x$titles, 
    #     "` := ", x$type, "(", 
    #     x$aboutUrl_eval, ", ",
    #     "base = '", x$aboutUrl_base, "', ",
    #     "datatype = '", x$datatype,
    #     "')]")

    eval(parse(text = string_to_eval), envir = parent.frame(1))
}

colnames_to_predicates = function(schema_list){
    if (! "propertyUrl" %in% colnames(schema_list$tableSchema$columns)){
        uriref(schema_list$tableSchema$columns$propertyUrl_eval, base = schema_list$`@context`[[2]]$`@base`)
    } else {
        ifelse(is.na(schema_list$tableSchema$columns$propertyUrl),
            uriref(schema_list$tableSchema$columns$column, base = schema_list$`@context`[[2]]$`@base`),
            paste0("<", schema_list$tableSchema$columns$propertyUrl, ">"))
    }
}

triples = function(df, schema_list){
    if (!is(df, "data.table")){
        stop("data is not a data table")
    }

    data.table::melt(df,
        measure.vars = list(
            as.character(schema_list$tableSchema$columns[, 'titles']),
            paste0(schema_list$tableSchema$columns[, 'titles'], "_sub")),
        id.vars = 'graph',
        variable.name = "pred",
        value.name = c("obj", "sub"),
        variable.factor = FALSE)
}
