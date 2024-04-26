
#' Dumpto FASTA file(s)
#'
#' @export
#' @param x table of NCBI search results with Species and Fasta columns or a named list of fastas
#' @param outpath chr, output path to save the file(s)
#' @param separate logical, if TRUE save one file per species otherwise save all to one file
#' @param filename chr, if separate is FALSE then write to this filename, otherwise
#'   write to files named in the pattern Species.fasta.  If a single file then please use the .gz extension.
#' @return the input table (unchanged)
dump_fasta = function(x, outpath = ".", separate = FALSE, filename = "dump.fasta.gz"){
  
  if (inherits(x, "data.frame")){
    if (separate){
      x = dplyr::rowwise(x) |>
        dplyr::group_walk(
          function(tbl, key){
            fname = file.path(outpath, sprintf("%s.fasta", tbl$Species))
            cat(tbl$Fasta, file = fname) 
          }
      )  
    } else {
      fname = file.path(outpath, filename)
      conn = gzfile(fname, open = 'wt')
      x = dplyr::rowwise(x) |>
        dplyr::group_walk(
          function(tbl, key){
            cat(tbl$Fasta, file = conn) 
          }
        ) 
      close(conn)
    }
  } else if (inherits(x, "list")){
    if (is.null(names(x))) stop("if input is a list of fastas, it must be named")
    if (separate){
      for (species in names(x)){
        fname = file.path(outpath, sprintf("%s.fasta", species))
        cat(x[[species]], file = fname) 
      }
    } else {
      fname = file.path(outpath, filename)
      conn = gzfile(fname, open = 'wt')
      for (contig in x){
        cat(contig, file = conn)
      }
      close(conn)
    }
  } else {
    stop("input must be a data frame with 'Species' and 'Fasta' columns or a named list of fastas")
  }
  invisible(x)
}