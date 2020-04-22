#' @import data.table
#' @export
nqwrite = function(dat, nquadpath, 
    append = FALSE, compress = TRUE){

    if (compress & stringi::stri_sub(nquadpath, -3, -1) != ".gz"){
        warning(paste("compress == ", compress, "and file", nquadpath, "does not end in .gz"))
    }

    if (append & !file.exists(nquadpath)){
        stop(paste("'append' is", append, ", but", nquadpath, "does not exist."))
    }

    if (compress) compress_par = "gzip" else compress_par = "none"

    # if (append) writemode = 'a' else writemode = 'w'

    # if (compress) {
    #     outfile = gzfile(nquadpath, open = writemode)
    # } else {
    #     outfile = file(nquadpath, open = writemode)
    # }

    # write.table(dat[complete.cases(pred, obj), list(sub, pred, obj, graph)], 
    #     file = outfile, sep = ' ', quote = F, 
    #     col.names = F, row.names = F, eol = " .\n")

    # close(outfile)
    data.table::fwrite(
        dat[complete.cases(pred, obj), list(sub, pred, obj, graph)],
        file = nquadpath,
        compress = compress_par,
        append = append,
        sep = " ", eol = " .\n", quote = FALSE,
        col.names = FALSE, row.names = FALSE)
}
