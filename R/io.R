#' @import data.table
nqwrite = function(dat, nquadpath, 
    append = FALSE, compress = TRUE){

    if (compress & stringi::stri_sub(nquadpath, -3, -1) != ".gz"){
        warning(paste("File", nquadpath, "does not end in .gz"))
    }

    if (append & !file.exists(nquadpath)){
        stop(paste("'append' is", append, ", but", nquadpath, "does not exist."))
    }

    if (append) writemode = 'a' else writemode = 'w'

    if (compress) {
        outfile = gzfile(nquadpath, open = writemode)
    } else {
        outfile = file(nquadpath, open = writemode)
    }

    write.table(dat[complete.cases(pred, obj), list(sub, pred, obj, graph)], 
        file = outfile, sep = ' ', quote = F, col.names = F, row.names = F)

    close(outfile)
}
