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
        sapply(dat, class), # should be xsd classes
        colnames(dat),
        paste0(schlist$`@id`, "/", colnames(dat)),
        colnames(dat),
        colnames(dat))
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

add_xsd = function(schema_list){
    datatypes = schema_list$`tableSchema`$columns$datatype
    datatypes = ifelse(stringi::stri_detect_fixed(datatypes, ":"), 
            datatypes, paste0("xsd:", datatypes))

    schema_list$`tableSchema`$columns$datatype = datatypes

    return(schema_list)
}

split_schema_uris = function(schema_list){

    # splits uri patterns into a uri part and the values that need to be drawn from columns
    # also recognise when base uri needs to be overwritten
}