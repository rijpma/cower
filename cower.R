rm(list = ls())

# separate namespace parser?
# todo: parse data.table statements from metadata file
# test perormance on append: acutally append or 

library("data.table")
library("jsonlite")
library("stringi")
library("urltools")

to_replace = "[ %+&]"
base = "https://iisg.amsterdam/resource/hsn/"
path_to_csv = "/Users/auke/repos/sdh-private-hsn/hsndbf/BEVDYNAP.DBF.csv.gz"
path_to_metadata = "/Users/auke/repos/sdh-private-hsn/hsndbf/BEVDYNAP.DBF.csv-metadata.json"
sub_path = 'bevdynap/'

literal = function(string, datatype = "xsd:string"){
    # needs to return NA
    ifelse(is.na(string), NA, paste0('"', string, '"^^', datatype))
}
uriref = function(string, base, path){
    ifelse(is.na(string), NA, 
    paste0("<", base, path, urltools::url_encode(gsub(to_replace, "_", string)), ">"))
}
mass = function(dt = "x", columns, type = c("uri", "literal"), paths, datatypes="xsd::string"){
    type = match.arg(type)
    if (type == "uri"){
        string = paste0(dt, "[, ", columns, ":= uriref(", columns, ", base = '", base, "', path = '", paths, "')]")
    } else if (type == "literal"){
        string = paste0(dt, "[, ", columns, ":= literal(", columns, ", datatype = '", datatypes, "')]")
    }
    eval(parse(text = string), envir = parent.frame(2))
}
bnode = function(n = 1){
    replicate(n, paste0("_:N", gsub("-", "", uuid::UUIDgenerate())))
     # N in nq serialisation to make NCName compliant-ish
}

# set all to character
x = data.table::fread(paste0("zcat < ", path_to_csv), colClasses = "character")

metadata = jsonlite::fromJSON(path_to_metadata)

# for (prefix in names(metadata$`@context`[[3]])){
#     metadata$tableSchema$columns[, c("propertyUrl", "valueUrl", "aboutUrl")] = 
#         lapply(metadata$tableSchema$columns[, c("propertyUrl", "valueUrl", "aboutUrl")], 
#             stringi::stri_replace_first_regex, paste0('^', prefix, ':'), metadata$`@context`[[3]][prefix])
# }

# go through file column by colum
# check encoding and characters
# create string (make function) or uri (make function)

# either
# x[, GEMEENTE := uriref(GEMEENTE, base = base, path = "gemeente/code/")]
# etc.

# or
# mass(dt = "x", columns = c("IDNR", "GEMEENTE"), type='uri', paths = c( 'idnr/code', 'gemeente/code/'))
# mass(dt = "x", columns = c("RELEASE", "DATUMCOR"), type='literal', datatypes= c( 'xsd:string', 'xsd:date'))

# or
uris = metadata$tableSchema$columns[!is.na(metadata$tableSchema$columns$valueUrl) & is.na(metadata$tableSchema$columns$virtual), ]
literals = metadata$tableSchema$columns[is.na(metadata$tableSchema$columns$valueUrl) & is.na(metadata$tableSchema$columns$virtual), ]

mass(dt = "x", 
    columns = uris$titles, 
    type='uri', 
    paths = gsub("hsn:", "", gsub("\\{.*", "", uris$valueUrl)))

mass(dt = "x", 
    columns = literals$titles, 
    type='literal', 
    datatypes = paste0("xsd:", gsub("xsd:", "", literals$datatype)))
x[, subj := uriref(.I, base = base, path = "bevadres/")]

# x[, pred := uriref(pred, base = base, path = 'dimension/')]


# create virtual columns (manual only possibility?)
# create additional datasets where hsnperson, idnr idnrhs etc. can be sub
# in separate dt!
virtual_meta = metadata$tableSchema$columns[metadata$tableSchema$columns$virtual & !is.na(metadata$tableSchema$columns$virtual), ]
virtual_meta[, c("aboutUrl", "propertyUrl", "valueUrl")]
virtuals = list(x[, list(subj = paste0(IDNR, "_", PERSNR), 
        pred = uriref("individualObservation", base = base, path = "dimension"),
         obj = subj)],
    x[, list(subj = IDNR,
        pred = uriref("OPHouseholdGroup", base = base, path = "dimension"),
        obj = IDNRHSP)],
    x[, list(subj = paste0(IDNR, "_", PERSNR),
        pred = uriref("inOPHouseholdGroup", base = base, path = "dimension"),
        obj = IDNR)],
    x[, list(subj = paste0(IDNR, "_", PERSNR),
        pred = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
        obj = uriref("individual", base = base, path = ""))],
    x[, list(subj = subj,
        pred = "<http://purl.org/linked-data/sdmx/2009/dimension#refArea>",
        obj = "<http://data.socialhistory.org/resource/clio/Netherlands/1880/1945>")])
virtuals = rbindlist(virtuals)
# rbindlist efficient?


names(x)[names(x) != 'subj'] = uriref(names(x)[names(x) != 'subj'], base = base, path = 'dimension/')

# melt (or is melt very bad performance-wise on larger dataset?)
# seems fine
x = data.table::melt(x, id.vars = "subj", 
    variable.name = "pred", 
    value.name = "obj")

# create predicate description 
uniq_preds = unique(x$pred)
uniq_preds = gsub('.*/|>', '', uniq_preds)
# back and forth: fix

locs_in_meta = match(uniq_preds, metadata$tableSchema$columns$titles)
uniq_preds = uniq_preds[!is.na(locs_in_meta)]
locs_in_meta = na.omit(locs_in_meta)


column_headers = uriref(uniq_preds, base = base, path = "bevadres.dbf.csv/column/name/")
description = literal(metadata$tableSchema$columns[locs_in_meta, "dc:description"])
datatype = metadata$tableSchema$columns[locs_in_meta, "datatype"]
datatype = uriref(datatype, base = "http://www.w3.org/2001/XMLSchema#", path ="")
name = literal(metadata$tableSchema$columns[locs_in_meta, "name"])
title = metadata$tableSchema$columns[locs_in_meta, "titles"]
title = literal(unlist(sapply(title, function(x) x[1])))
predicates = melt(data.table(subj = column_headers, description, datatype, name, title), 
    id.vars = "subj", variable.name = "pred", value.name = "obj")

predicates[, pred := ifelse(pred == "description", uriref(pred, base = "http://purl.org/dc/terms/", path = ""), uriref(pred, "http://www.w3.org/ns/csvw#", path = ""))]
    # bnode1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> # resource/originalfile/column/name
    # bnode1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> bnode2
# http://www.w3.org/ns/csvw#valueUrl  
#  http://data.socialhistory.org/resource/hsn/code/Gemeente/__GEMEENTE_title()__ 
    # bnode propertyUrl predicate
    # bnode dc:desc desc


# x[, pred := uriref(pred, base = base, path = 'dimension/')]
# this is dog slow
# fix? uriref on the colnames before melt?

# rbind.datalist(data, virtual, descriptions)
# , meta)
# add hash

filehash = system(paste0("git hash-object ", path_to_csv), intern = TRUE)
# fails quietly!
# hash of the compressed file now...!?


now = format(Sys.time(), "%Y-%m-%dT%H:%M")
nanopub = paste0("<", base, sub_path, c('assertion/', 'nanopublication/', 'provenance/', 'pubinfo/', 'resource/'), substring(filehash, 1, 8), "/", now, ">")
names(nanopub) = gsub(paste0("/", substring(filehash, 1, 8), paste0('.*|.*', sub_path)), '', nanopub)

x[, graph := nanopub["assertion"]]

meta = rbindlist(list(data.table(nanopub['assertion'], 
        c('a', 'generatedAtTime', 'wasDerivedFrom'), 
        c("nanopub:assertion", now, nanopub['resource'])),
    data.table(nanopub['nanopublication'],
        c('hasAssertion', 'hasProvenance', 'hasPubinfo', 'a', 'generatedAtTime', 'wasGeneratedBy'),
        c(nanopub['assertion'], nanopub['provenance'], nanopub['pubinfo'], 'nanonpublication', now, 'RRRRRR')),
    data.table(nanopub['provenance'], 'a', 'nanopub:provenance'),
    data.table(nanopub['pubinfo'], 'a', 'nanopub:publicationinfo'),
    data.table(nanopub['resource'], 
        c('http://data.socialhistory.org/vocab/path', '<http://data.socialhistory.org/vocab/sha1_hash>'),
        c(literal(path_to_csv), literal(filehash)))))
names(meta) =  c("subj", "pred", "obj")
# etc.
# meta[, grap := ]

predicates[, graph := nanopub['provenance']]

# speed and memory testing required here, doubles memory usage for small write
# write separately?
# swap sample and complete.cases for speed?
# separate
outfile = gzfile("~/desktop/test.nq.gz", 'w')
write.table(x[complete.cases(pred, obj), list(subj, pred, obj, graph)][sample(.N, 1e5), ], 
    file = outfile, sep = ' ', quote = F, col.names = F, row.names = F)
close(outfile)

outfile = "~/desktop/test.nq")
write.table(x[complete.cases(pred, obj), list(subj, pred, obj, graph)][sample(.N, 1e5), ], 
    file = outfile, sep = ' ', quote = F, col.names = F, row.names = F)


rbindlist(list(x, virtuals, predicates, meta))[complete.cases(pred, obj), list(subj, pred, obj)]
data.table::fwrite(out[complete.cases(pred, obj),  list(subj, pred, obj)], '~/desktop/test.nq', sep = ' ', quote=F, col.names=F)

# make metagraphs
    # s p o assertion
    # assertion a nanopub:assertion
    #     generatedAtTime time
    #     wasDerivedFrom resource
    # nanopub hasAssertion assertion
    #     hasProvenance provenance
    #     hasPubinfo publinfo
    #     a nanopublication
    #     generatedAtTime time
    #     wasGeneratedBy wp4-converters -> R
    # provenance a nanopub:provenance
    # pubinfo a publicationinfo
    # resource vocab/path filepath
    #     vocab/sha1_hash hash (shared everywhere)

names(x)
head(metadata$tableSchema$columns)
x[, gsub("\\{.*", IDNR, metadata$tableSchema$columns[metadata$tableSchema$columns$titles == "IDNR", "valueUrl"])]
x[, stringi::stri_replace_all_regex(metadata$tableSchema$columns[metadata$tableSchema$columns$titles == "IDNR", "valueUrl"], "\\{.*", IDNR)]
x[, uriref(IDNR, base = base, path = "code/idnr/")]
x[, HUISHNR := literal(HUISHNR)]

# for accessing metadata 
# so get all datatype == string and is.na(valueUrl)
# do this
x[, ("HUISHNR") := literal(get("HUISHNR"))]


vrb = c("IDNR", "GEMEENTE")
further_path = c("idnr/code/", 'gemeente/code/')
y = copy(x)
# for all !is.na(valueUrl)
system.time(y[, (vrb) := uriref(get(vrb), base = base, path = further_path)])
head(y[, list(IDNR, GEMEENTE)])

# works with multiple variables, could also recycle
rm(y)
y = copy(x)
system.time(eval(parse(text = paste0("y[, ", vrb, ":= uriref(", vrb, ", base = '", base, "', path = '", further_path, "')]"))))
head(y[, list(IDNR, GEMEENTE)])

# set does not work with multiple vrbs
rm(y)
y = copy(x)
system.time(data.table::set(y, j = vrb, value = uriref(y[[vrb]], base = base, path = further_path)))
head(y[, list(IDNR, GEMEENTE)])



virtual = x[, list(IDNR, "OPHouseholdGroup", IDNRHS)]
virtual = x[, list(hsnPerson = paste0(IDNR, PERSNR), "OPHouseholdGroup", IDNRHS)]

x[, rownum := .I]
x = melt(x, id.vars = 'rownum')
x[, variable := uriref(variable, base="https://iisg.amsterdam/resource/hsn/", namespace = "dimension/")]

predicates = unique(x[, list(rownum = variable, variable = "dc:description", value = "")])
predicates[rownum == "<https://iisg.amsterdam/resource/hsn/dimension/IDNR>", value := "description"]
rbind(x, predicates)






value = c('a', 'b', 'c')


# x = data.table::fread("~/repos/sdh-private-hsn/hsndbf1000/bevadres.dbf.csv")
x = data.table::as.data.table(foreign::read.dbf("/Users/auke/Downloads/data/hsndata/BEVADRES.DBF"))
x[, rownum := .I]

print(object.size(x), units="MB")
system.time(test <- melt(x, id.vars = "rownum"))
rm(test)

x = x[, lapply(.SD, uriref, base = "http://data.socialhistory.org/resource/hsn/code/")]
print(object.size(x), units="MB")
system.time(test <- melt(x, id.vars = "rownum"))



literal(value, 'xsd:string')

metadata = jsonlite::fromJSON("~/repos/sdh-private-hsn/hsndbf1000/bevadres.dbf.csv-metadata.json")
# x = data.table::as.data.table(foreign::read.dbf("/Users/auke/Downloads/data/hsndata/BEVDYNAP.DBF"))
# x = data.table::fread("~/repos/sdh-private-hsn/hsndbf1000/bevadres.dbf.csv")
x[, rownum := .I]
# metadata$tableSchema$columns[, c('virtual')]

for (prefix in names(metadata$`@context`[[3]])){
    metadata$tableSchema$columns[, c("propertyUrl", "valueUrl", "aboutUrl")] = 
        lapply(metadata$tableSchema$columns[, c("propertyUrl", "valueUrl", "aboutUrl")], 
            stringi::stri_replace_first_regex, paste0('^', prefix, ':'), metadata$`@context`[[3]][prefix])
}

# metadata$tableSchema$columns[, c('name', 'propertyUrl')]
# metadata$tableSchema$columns[, c('name', 'valueUrl')]
# metadata$tableSchema$columns[, c('name', 'datatype')]
print(object.size(x), units='MB')

x = melt(x, id.var='rownum')
tools::showNonASCII(x[!is.na(value), unique(value)])

# x[, aboutUrl := stringi::stri_replace_all_fixed(metadata$tableSchema$aboutUrl, '{_row}', rownum)]
x[, subject := stringi::stri_paste(metadata$`@context`[[2]]$`@base`, 'BEVADRES/', rownum)]
x[, rownum := NULL]
print(object.size(x), units='MB')


x[, datatype := metadata$tableSchema$columns$datatype[match(variable, metadata$tableSchema$columns$name)]]
x[, valueUrl := metadata$tableSchema$columns$valueUrl[match(variable, metadata$tableSchema$columns$name)]]

x[, predicate := metadata$tableSchema$columns$propertyUrl[match(variable, metadata$tableSchema$columns$name)]]

unique(x$datatype)
x[datatype == "string" & is.na(valueUrl) & !is.na(value), object := paste0('"', value, '"', "^^xsd:string")]
x[datatype == "int" & is.na(valueUrl) & !is.na(value), object := paste0('"', value, '"', "^^xsd:int")]
x[datatype == "float" & is.na(valueUrl) & !is.na(value), object := paste0('"', value, '"', "^^xsd:float")]
x[datatype == "integer" & is.na(valueUrl) & !is.na(value), object := paste0('"', value, '"', "^^xsd:integer")]
x[datatype == "xsd:date" & is.na(valueUrl) & !is.na(value), object := paste0('"', value, '"', "^^xsd:date")]
x[datatype == "xsd:gYear" & is.na(valueUrl) & !is.na(value), object := paste0('"', value, '"', "^^xsd:gYear")]

x[, variable := NULL]
x[, datatype := NULL]

badchars = "[\\^\\'\\-\\.\\ \\%\\@\\$\\*\"\\ \\!\\`\\/\\:\\,\\(\\)\\+]"
x[!is.na(valueUrl), value := stringi::stri_replace_all_regex(value, badchars, '_')]
x[!is.na(valueUrl), object := URLencode(stringi::stri_replace_all_regex(valueUrl, "\\{+.*\\}+", value))]


x[complete.cases(predicate, object), list(subject, predicate, object)]
unique(x[, list(predicate, object)])
data.table::fwrite(x[complete.cases(predicate, object),  list(subject, predicate, object)], '~/desktop/test.nq', sep = '', quote=F)

# todo: about url misses @abouturl
# todo: get xsd:etc outside the quote marks -- write.table|fwrite(... quote = FALSE)
# todo: check which bad characters need escaping
# wrap all non-strings in <>: all sub pred and all obj where url
# nquad 

unique(x$object)




for (prefix in metadata$`@context`[[3]]){
    x[, lapply(.SD, stringi::stri_replace_first_fixed, paste0('^', names(prefix)), prefix)]
}

x[, metadata$`@context`[[3]][stringi::stri_sub(stringi::stri_extract_first_regex(predicate, '.*\\:'), to = -2)]]
x[, stringi::stri_subset_fixed(predicate, names(metadata$`@context`[[3]]))]

x[, list(subject, predicate, object)]

x[, predicate := metadata$`@context`[[3]][stringi::stri_sub(stringi::stri_extract_first_regex(predicate, '.*\\:'), to = -2)]]
x[, subject := metadata$`@context`[[3]][stringi::stri_sub(stringi::stri_extract_first_regex(subject, '.*\\:'), to = -2)]]
x[, object := metadata$`@context`[[3]][stringi::stri_sub(stringi::stri_extract_first_regex(object, '.*\\:'), to = -2)]]

x[, propertyUrl := paste0('http://www.example.org/bla/', variable)]
x[, valueUrl := value]

print(object.size(x), units='MB')
