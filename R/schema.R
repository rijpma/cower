read_json_schema =function(jsonpath){
    # json in, metadata list out
    schema_list = jsonlite::fromJSON(jsonpath)
    # tests go here?

    return(schema_list)
}

get_namespaces = function(schema_list){
    namespaces = unlist(schema_list$`@context`[[3]]) # pos 3 fixed?
    names(namespaces) = paste0(names(namespaces), ":")
    return(namespaces)
}

#' @export
expand_prefixes = function(schema_list, context){
    # also expands context part. 
    # data.frame becomes matrix.
    if (!is(schema_list, "list")){
        # print(schema_list)
        return(sapply(schema_list, 
            function(x) stringi::stri_replace_all_regex(x, 
                stringi::stri_join("^", names(context)), 
                context,
            vectorize_all = F)))
    }
    lapply(schema_list, expand_prefixes, context) 
}

add_namespaces = function(table_schema, base){
    # insert missing namespaces: xsd for datatype, base for *Url

    table_schema$datatype = ifelse(
        stringi::stri_detect_fixed(table_schema$datatype, ":"), 
        table_schema$datatype, 
        paste0("http://www.w3.org/2001/XMLSchema#", table_schema$datatype))

    urlcolumns = colnames(table_schema)[grep("Url$", colnames(table_schema))]
    table_schema[, urlcolumns] = lapply(table_schema[, urlcolumns, drop = F], 
        function(x) {
            ifelse(
                stringi::stri_detect_fixed(x, ":"), 
                x, 
                paste0(base, x)
            )
        }
    )

    return(table_schema)
}

fix_abouturl = function(schema_list){
    # add base to aboutUrls without namespace

    schema_list$tableSchema$aboutUrl = ifelse(
        stringi::stri_detect_fixed(schema_list$tableSchema$aboutUrl, ":"),
        schema_list$tableSchema$aboutUrl,
        paste0(schema_list$`@context`[[2]]$`@base`, schema_list$tableSchema$aboutUrl)
    )
    return(schema_list)
}

split_schema_uris = function(table_schema){
    # split out fixed base and variable column values,

    urlcolumns = colnames(table_schema)[grep("Url$", colnames(table_schema))]
    # urlcolumns = "valueUrl"

    if (length(urlcolumns) == 0){
        return(table_schema)
    }

    table_schema[, paste0(urlcolumns, "_base")] = 
        lapply(table_schema[, urlcolumns, drop = F], 
            function(x) unlist(tstrsplit(x, "\\{{1,2}", keep = 1)))
    table_schema[, paste0(urlcolumns, "_eval")] = 
        lapply(table_schema[, urlcolumns, drop = F], 
            stringi::stri_extract_first_regex, "\\{{1,2}.*\\}{1,2}")
    # tstrsplit only usable here because keep cannot be two if there's nothing to split in column

    return(table_schema)
}


add_schema_evals = function(table_schema, global_aboutUrl){
    # make sure all columns exist
    if (! "valueUrl_base" %in% names(table_schema)) table_schema$valueUrl_base = NA
    if (! "aboutUrl_base" %in% names(table_schema)) table_schema$aboutUrl_base = NA
    if (! "propertyUrl_base" %in% names(table_schema)) table_schema$propertyUrl_base = NA
    if (! "valueUrl_eval" %in% names(table_schema)) table_schema$valueUrl_eval = NA
    if (! "aboutUrl_eval" %in% names(table_schema)) table_schema$aboutUrl_eval = NA
    if (! "propertyUrl_eval" %in% names(table_schema)) table_schema$propertyUrl_eval = NA
    if (! "csvw:value" %in% names(table_schema)) table_schema$`csvw:value` = NA

    # fill in type based on presence valueUrl
    if ("valueUrl" %in% names(table_schema)){
        table_schema$type = ifelse(is.na(table_schema$valueUrl), "literal", "uriref")
    } else {
        table_schema$type = "literal"
    }

    # replace eval by csvw:value if missing
    table_schema$valueUrl_eval = ifelse(is.na(table_schema$valueUrl_eval) & !is.na(table_schema$`csvw:value`), table_schema$`csvw:value`, table_schema$valueUrl_eval)
    # replace eval by empty string if missing and not csvw:value and uriref
    table_schema$valueUrl_eval = ifelse(is.na(table_schema$valueUrl_eval) & table_schema$type == "uriref", "\"\"", table_schema$valueUrl_eval)
    # use name for eval if still missing
    table_schema$valueUrl_eval = ifelse(is.na(table_schema$valueUrl_eval), table_schema$name, table_schema$valueUrl_eval)
    # use column title for eval if completely unspecified
    table_schema$valueUrl_eval = ifelse(is.na(table_schema$valueUrl_eval), table_schema$column, table_schema$valueUrl_eval)

    # if propertyUrl_eval missing, set to "" to prevent NA
    table_schema$propertyUrl_eval = ifelse(is.na(table_schema$propertyUrl_eval), "\"\"", table_schema$propertyUrl_eval)

    # replace {_row} with .I (why not stri_replace_last_fixed) ?
    table_schema[, grep("_eval$", names(table_schema))] = 
        lapply(
            table_schema[, grep("_eval$", names(table_schema)), drop = F],
                stringi::stri_replace_all_regex, "\\{{1,2}_row\\}{1,2}|^_row$", 
            ".I"
        )

    # strip parenthesis on remaining _eval bits
    table_schema[, grep("_eval$", names(table_schema))] = 
        lapply(table_schema[, grep("_eval$", names(table_schema)), drop = F], 
            stringi::stri_replace_all_fixed, c("{", "}"), "", vectorize_all = F)

    return(table_schema)
}

fix_missing_virtuals = function(table_schema){
    # add virtual column if missing and set to false if not specified for column (NA)
    if (! "virtual" %in% colnames(table_schema)){
        table_schema$virtual = FALSE
    }
    table_schema$virtual = ifelse(
        is.na(table_schema$virtual),
        FALSE, 
        table_schema$virtual)

    return(table_schema)

}

fix_empty_titles = function(table_schema){
    # [] on titles reads as list in df and becomes NULL, 

    if (! "titles" %in% colnames(table_schema)){
        table_schema$titles = ""
    }

    if (any(
            table_schema$virtual == FALSE 
            & (sapply(table_schema$titles, is.null) 
                | is.na(table_schema$titles) 
                | table_schema$titles == ""))){
            warning("Non-virtual column missing title, creating random column name")
            # surely it always does this?
    }

    # separate column description where duplicates are allowed
    # while keeping titles in place for correct predicate placement
    table_schema$column = table_schema$titles
    table_schema$titles = stringi::stri_join("newcol", 1:nrow(table_schema), stringi::stri_rand_strings(n = nrow(table_schema), length = 10, pattern = '[a-z]'))
    table_schema$titles = make.unique(table_schema$titles)

    return(table_schema)
}

add_subject_base = function(table_schema, base){
    if (is.null(table_schema$aboutUrl_base)){
        table_schema$aboutUrl_base = base
    } else {
        table_schema$aboutUrl_base = ifelse(
            is.na(table_schema$aboutUrl_base), 
            base,
            table_schema$aboutUrl_base)
    }

    return(table_schema)
}

insert_null = function(table_schema){
    if (is.null(table_schema$null)){
        table_schema$null = vector("list", nrow(table_schema))
    }

    return(table_schema)
}

insert_emptyurls = function(table_schema, global_abouturl, base){
    if (is.null(table_schema$aboutUrl)){
        table_schema$aboutUrl = global_abouturl
    } else {
        table_schema$aboutUrl = ifelse(is.na(table_schema$aboutUrl), global_abouturl, table_schema$aboutUrl)
    }

    if (is.null(table_schema$propertyUrl)){
        table_schema$propertyUrl = paste0(base, table_schema$name) # tofix: something consistent
    } else {
        table_schema$propertyUrl = ifelse(is.na(table_schema$propertyUrl), paste0(base, table_schema$name), table_schema$propertyUrl)
    }
    
    return(table_schema)
}

datatypes_as_urirefs = function(schema_list){
    # done on schema_list because non-expanded prefixes might still exist
    datatypes = schema_list$tableSchema$columns$datatype
    datatypes = ifelse(
        grepl("^@", datatypes) | is.na(datatypes), 
        datatypes, 
        uriref(rep("", length(datatypes)), base = datatypes))
    schema_list$tableSchema$columns$datatype = datatypes
    return(schema_list)
}

prep_table_schema = function(schema_list){

    table_schema = as.data.frame(schema_list$tableSchema$columns, stringsAsFactors = FALSE)

    table_schema = fix_missing_virtuals(table_schema = table_schema)
    table_schema = fix_empty_titles(table_schema = table_schema)
    table_schema = add_namespaces(table_schema, base = schema_list$`@context`[[2]]$`@base`)
    table_schema = insert_null(table_schema)
    table_schema = insert_emptyurls(table_schema, 
        global_abouturl = schema_list$tableSchema$aboutUrl, 
        base = schema_list$`@context`[[2]]$`@base`)
    table_schema = split_schema_uris(table_schema)
    table_schema = add_subject_base(table_schema, base = schema_list$`@context`[[2]]$`@base`)
    table_schema = add_schema_evals(table_schema)

    schema_list$tableSchema$columns = table_schema
    return(schema_list)
}

xsd_from_datatypes = function(datatypes){
    dct = c("character" = "string",
        "integer" = "integer",
        "numeric" = "float",
        "double" = "float",
        "logical" = "boolean")

    return(dct[datatypes])
}
