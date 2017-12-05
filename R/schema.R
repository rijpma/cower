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

expand_prefixes(schema_list){
    # take schema_list and expend prefixes in tableSchema based on namespaces
    return(expanded_schema_list)
}

split_schema_uris(schema_list){
    # splits uri patterns into a uri part and the values that need to be drawn from columns
    # also recognise when base uri needs to be overwritten
}