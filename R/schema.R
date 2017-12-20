# schema.R
build_schema_list = function(csvpath){
    # create schema list from dat
    return(schemalist)
}

schema_json = function(schema_list){
    # make json file from schema list
    write(json)
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

split_schema_uris = function(schema_list){
    # splits uri patterns into a uri part and the values that need to be drawn from columns
    # also recognise when base uri needs to be overwritten
}