graph_names = function(path_to_csv, base = "https://iisg.amsterdam/resource/hsn/", sub_path){

    filehash = git2r::hashfile(path_to_csv)
    now = format(Sys.time(), "%Y-%m-%dT%H:%M")
    graphs = c('assertion', 'nanopublication', 'provenance', 'pubinfo', 'resource')

    # better: use uriref
    nanopub = paste0("<", base, sub_path, graphs, "/", 
         substring(filehash, 1, 8), "/", now, ">")

    names(nanopub) = graphs

    return(nanopub)
}

