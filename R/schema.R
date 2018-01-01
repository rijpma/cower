# schema.R
build_schema_list = function(csvpath,
    delimiter = ",", encoding = "UTF-8", 
    base = "https://iisg.amsterdam/resource/", 
    dataset_name = ""){

    dat = data.table::fread(csvpath, nrows = 100)

    schlist = list()

    schlist$dialect = list()

    schlist$dialect$quoteChar = "\"" # take input from fread
    schlist$dialect$delimiter = delimiter # take input from fread
    schlist$dialect$encoding = encoding # necessary with fread? needs close look

    schlist$`dcat:keyword` = list()

    schlist$`dc:license` = "http://opendefinition.org/licenses/cc-by/"

    schlist$`dc:publisher` = list()
    schlist$`dc:publisher`$`schema:name` = "CLARIAH Structured Data Hub - Datalegend"

    schlist$`dc:publisher`$`schema:url` = list()
    schlist$`dc:publisher`$`schema:url`$`schema:url`$`@id` = "http://datalegend.org"

    schlist$`url` = basename(csvpath)

    schlist$`@context` = list()
    schlist$`@context`[[1]] = "http://csvw.clariah-sdh.eculture.labs.vu.nl/csvw.json"
    schlist$`@context`[[2]] = list("@base" = base, "@language" = "en")
    schlist$`@context`[[3]] = yaml::read_yaml("https://raw.githubusercontent.com/CLARIAH/COW/master/cow/converter/util/namespaces.yaml")
    # not sure if this is where COW gets the list

    if (dataset_name == ""){
        schlist$`dc:title` = basename(csvpath)
    } else {
        schlist$`dc:title` = dataset_name
    }

    # need to ensure that base ends in /
    schlist$`@id` = paste0(base, basename(csvpath)) 

    schlist$`dc:modified` = list()
    schlist$`dc:modified`$`@value` = format(Sys.time(), "%Y-%m-%d")

    schlist$`tableSchema` = list()
    schlist$`tableSchema`$aboutUrl = "{_row}"
    schlist$`tableSchema`$primaryKey = colnames(dat)[1]
    schlist$`tableSchema`$columns = data.frame(
        xsd_from_datatypes(sapply(dat, class)), # should be xsd classes
        colnames(dat),
        paste0(schlist$`@id`, "/", colnames(dat)),
        colnames(dat),
        colnames(dat), stringsAsFactors = FALSE)
    names(schlist$`tableSchema`$columns) = c("datatype", "titles", "@id", "name", "dc:description")
    rownames(schlist$`tableSchema`$columns) = NULL

    return(schlist)
}

schema_json = function(schema_list, jsonpath){
    schema_as_json = jsonlite::toJSON(schema_list,
        pretty = TRUE, auto_unbox = TRUE)
    writeLines(schema_as_json, jsonpath)
}

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

expand_prefixes = function(schema_list, context){
    # also expands context part. 
    # data.frame becomes matrix.
    # Problem?
    if (!is(schema_list, "list")){
        # print(schema_list)
        return(sapply(schema_list, 
            function(x) stringi::stri_replace_all_fixed(x, 
                names(context), 
                context,
            vectorize_all = F)))
    }
    lapply(schema_list, expand_prefixes, context) 
}

add_xsd = function(table_schema){
    table_schema$datatype = ifelse(
        stringi::stri_detect_fixed(table_schema$datatype, ":"), 
            table_schema$datatype, 
            paste0("http://www.w3.org/2001/XMLSchema#", table_schema$datatype))

    return(table_schema)
}

split_schema_uris = function(table_schema){
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
            stri_extract_first_regex, "\\{{1,2}.*\\}")
    # tstrsplit only usable here because keep cannot be two if there's nothing to split in column

    table_schema[, paste0(urlcolumns, "_eval")] = 
        lapply(table_schema[, paste0(urlcolumns, "_eval"), drop = F], 
        stri_replace_all_fixed, c("{", "}"), "", vectorize_all = F)

    return(table_schema)
}


add_schema_evals = function(table_schema){
    if (! "valueUrl_base" %in% names(table_schema)) table_schema$valueUrl_base = NA
    if (! "aboutUrl_base" %in% names(table_schema)) table_schema$aboutUrl_base = NA
    if (! "propertyUrl_base" %in% names(table_schema)) table_schema$propertyUrl_base = NA
    if (! "valueUrl_eval" %in% names(table_schema)) table_schema$valueUrl_eval = NA
    if (! "aboutUrl_eval" %in% names(table_schema)) table_schema$aboutUrl_eval = NA
    if (! "propertyUrl_eval" %in% names(table_schema)) table_schema$propertyUrl_eval = NA


    if ("valueUrl" %in% names(table_schema)){ # anyUrl?
        table_schema$type = ifelse(is.na(table_schema$valueUrl), "literal", "uriref")
    } else {
        table_schema$type = "literal"
    }

    table_schema$aboutUrl_eval = ifelse(is.na(table_schema$aboutUrl_eval), ".I", table_schema$aboutUrl_eval)

    table_schema$aboutUrl_eval[table_schema$aboutUrl_eval == "{_row}"] = ".I"

    table_schema$propertyUrl_eval = ifelse(is.na(table_schema$propertyUrl_eval), 
        table_schema$titles, 
        table_schema$propertyUrl_eval)

    table_schema$valueUrl_eval = 
        ifelse(table_schema$type == "literal", 
            table_schema$titles, 
            table_schema$valueUrl_eval)
    # table_schema$propertyUrl_eval = 
    #     ifelse(is.na(table_schema$propertyUrl), 
    #         table_schema$titles, 
    #         table_schema$propertyUrl)

    return(table_schema)
}

fix_missing_virtuals = function(table_schema){
    if (! "virtual" %in% colnames(table_schema)){
        table_schema$virtual = FALSE
    }
    table_schema$virtual = ifelse(is.na(table_schema$virtual),
        FALSE, table_schema$virtual)

    return(table_schema)

}

fix_null_titles = function(table_schema){
    # [] on titles reads as list in df and becomes NULL,
    # replace with temp colname

    table_schema$titles[sapply(table_schema$titles, is.null)] = 
        paste0("V", 1:sum(!is.null(table_schema$titles)))

    table_schema$titles = unlist(table_schema$titles)

    return(table_schema)
}

add_subject_base = function(table_schema, base){
    if (is.null(table_schema$aboutUrl_base)){
        table_schema$aboutUrl_base = base
    } else {
        table_schema$aboutUrl_base = ifelse(is.na(table_schema$aboutUrl_base), 
            base,
            table_schema$aboutUrl_base)
    }

    return(table_schema)
}

prep_table_schema = function(schema_list){
    table_schema = as.data.frame(schema_list$tableSchema$columns, stringsAsFactors = FALSE)

    table_schema = fix_missing_virtuals(table_schema = table_schema)
    table_schema = fix_null_titles(table_schema = table_schema)
    table_schema = add_xsd(table_schema)
    table_schema = split_schema_uris(table_schema)
    table_schema = add_subject_base(table_schema, base = schema_list$`@context`[[2]]$`@base`)
    table_schema = add_schema_evals(table_schema)

    schema_list$tableSchema$columns = table_schema
    return(schema_list)
}

xsd_from_datatypes = function(datatypes){
    dct = c("character" = "string",
        "numeric" = "float",
        "double" = "float",
        "logical" = "boolean")

    return(dct[datatypes])
}
