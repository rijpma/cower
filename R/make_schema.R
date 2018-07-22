#' Create a metadata skeleton list from a csv dataset to make a JSON-LD description.
#'
#' Create a metadata skeleton list from a csv dataset to make a JSON-LD description.
#'
#' Obtain a JSON-LD description by writing the resulting list to JSON using
#' \code{\link{write_schema_json}}. It is not recommended to edit the list in R,
#' but rather to edit that JSON file to fully describe the csv file.
#' @param csvpath Path to the csv file for which to create JSON-LD metadata.
#' @param delimiter Delimiter of the file. Default is \code{"auto"} to let
#' \code{data.table::fread} guess the delimiter. \code{fread} is good at that,
#' but \code{"auto"} is not what the csvw-standard expects.
#' @param encoding The encoding of the csv-file. Note that
#' data.table::fread used elsewhere only acccepts UTF-8 and Latin1.
#' @param base The base for the URIs that are to be created. You can also
#' change this in the JSON file.
#' @param dataset_name The name of the dataset. If empty (default),
#' defaults to the basename of the csv file.
#' @return A list describing the csv file.
#' @import data.table
#' @export
build_schema_list = function(csvpath,
    delimiter = "auto", encoding = "UTF-8",
    base = "https://iisg.amsterdam/resource/",
    dataset_name = ""){

    dat = data.table::fread(csvpath, nrows = 100)

    schlist = list()

    schlist$dialect = list()

    schlist$dialect$quoteChar = "\"" # take input from fread
    schlist$dialect$delimiter = delimiter
    schlist$dialect$encoding = encoding # necessary with fread? needs close look

    schlist$`dcat:keyword` = list()

    schlist$`dc:license` = list()
    schlist$`dc:license`$`@id` = "http://opendefinition.org/licenses/cc-by/"

    schlist$`dc:publisher` = list()
    schlist$`dc:publisher`$`schema:name` = "CLARIAH Structured Data Hub - Datalegend"

    schlist$`dc:publisher`$`schema:url` = list()
    schlist$`dc:publisher`$`schema:url`$`@id` = "http://datalegend.org"

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

#' Write basic JSON-LD metadata to file.
#' @export
write_schema_json = function(schema_list, jsonpath){
    schema_as_json = jsonlite::toJSON(schema_list,
        pretty = TRUE, auto_unbox = TRUE)
    writeLines(schema_as_json, jsonpath)
}
