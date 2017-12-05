# schema.R
build_schema_list(csvpath){
    # create schema list from dat
    return(schemalist)
}

schema_json(schema_list){
    # make json file from schema list
    write(json)
}

read_json_schema(jsonpath){
    # json in, metadata list out
    schema_list = jsonlite::fromJSON(jsonpath)
    # tests go here?

    return(schema_list)
}

expand_prefixes = function(schema_list){
    # take schema_list and expend prefixes in tableSchema based on namespaces
    
    # add xsd: if no prefix in datatype
    datatypes = schema_list$tableSchema$columns$datatype
    schema_list$tableSchema$columns$datatype = ifelse(grepl(":", datatypes), 
        datatypes, paste0("xsd:", datatypes))

    namespaces = unlist(schema_list$`@context`[[3]]) # is this always in position 3?
    for (i in 1:length(namespaces)){
        namespace_pattern = paste0("^", names(namespaces[i]), ":")

        for (j in 1:ncol(schema_list$tableSchema$columns)){
            schema_list$tableSchema$columns[, j] = gsub(pattern = namespace_pattern, 
                replacement = namespaces[i], 
                x = schema_list$tableSchema$columns[, j])
        }        
    }
    # also aboutUrl and elsewhere in schema_list. 

    return(schema_list)
}

split_schema_uris(schema_list){
    # splits uri patterns into a uri part and the values that need to be drawn from columns
    # also recognise when base uri needs to be overwritten
}