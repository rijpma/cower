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
#' @export
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
#' @export
uriref = function(string, base, path = '', ...){
    to_replace = "[ %+&{}]"

    ifelse(is.na(string), NA, 
    paste0("<", base, path, urltools::url_encode(gsub(to_replace, "_", string)), ">"))
}

#' Create blank node
#' @export
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
#' @param dat Name of the data.table as a length one character vector.
#' @param schema_list The schema list describing the csv.
#' @param done_so_far Counter to offset row names in case of batched conversion
#' @examples
convert = function(dat, schema_list, done_so_far = 0){

    dat = deparse(substitute(dat))

    table_schema = schema_list$tableSchema$columns

    table_schema$aboutUrl_eval = stringi::stri_replace_all_fixed(table_schema$aboutUrl_eval, ".I", stringi::stri_join("(.I + " , + done_so_far, ")"))

    # if length of array in json == 1, expand_prefixes() in its to-matrix
    # conversion does not turn into backslash quoted vectors. Needs a fix that
    # does not depend on strange to-matrix conversion
    table_schema$null = ifelse(
        test = table_schema$null == "NULL" | stringi::stri_detect_fixed(table_schema$null, '\"'),
        yes = table_schema$null,
        no = paste0('"', table_schema$null, '"')
    )


    filllist = list()
    for (i in 1:nrow(table_schema)){

        # print(table_schema$aboutUrl_eval[i]) # + aboutUrl_base + done_so_far stuff for chuncking
        # print(table_schema$propertyUrl_eval[i]) # is.na base schema_list$`@context`[[2]]$`@base` + colnames, propertyUrl_eval
        # print(table_schema$valueUrl_eval[i])
        about = paste0(
            dat,
            "[!",                       # nullstring
            table_schema$name[1],
            " %in% ",
            table_schema$null[2],
            ", ",
            "uriref", "(",             # column
            "base",    " = '", table_schema$aboutUrl_base[i], "', ",
            "string",   " = ",  table_schema$aboutUrl_eval[i],
            ")]")
        property = paste0(
            dat,
            "[!",                       # nullstring
            table_schema$name[1],
            " %in% ",
            table_schema$null[2],
            ", ",
            "uriref", "(",             # column
            "base",    " = '", table_schema$propertyUrl_base[i], "', ",
            "string",   " = ",  table_schema$propertyUrl_eval[i], # cannot be NA or NA returned
            ")]")
        value = paste0(
            dat,
            "[!",                       # nullstring
            table_schema$name[1],
            " %in% ",
            table_schema$null[2],
            ", ",
            table_schema$type[i], "(", # column
            "base",    " = '", table_schema$valueUrl_base[i], "', ",
            "string",   " = ",  table_schema$valueUrl_eval[i], ", ",
            "datatype"," = '", table_schema$datatype[i],      "')]") # quietly ignored if literal

        # quoted NA into proper NA
        about = stringi::stri_replace_all_fixed(about, "'NA'", "NA")
        property = stringi::stri_replace_all_fixed(property, "'NA'", "NA")
        value = stringi::stri_replace_all_fixed(value, "'NA'", "NA")

        filllist[[i]] = data.table(
            sub =  eval(parse(text = about), envir = parent.frame(1)), 
            pred = eval(parse(text = property), envir = parent.frame(1)), 
            obj =  eval(parse(text = value), envir = parent.frame(1))
        )
    }

    # and now do something on filllist

    # because NA ignored with %in%
    # nastring = ifelse(x$null == "NULL",
    #     "",
    #     paste0(
    #         dat, "[",
    #         "is.na(", x$column, ")", 
    #         ", `", x$newvar, "`",
    #         " := ", "NA",
    #         "]"
    #         ))
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
