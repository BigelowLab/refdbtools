# See https://github.com/Maine-eDNA/12sRef/blob/main/GOM_Metazoan_COI_script.Rmd

#' Search of target species at NCBI
#' 
#' @export
#' @param cfg configuration list
#' @param entrez_key character, see \code{\link{get_entrez_key}}
#' @return list of voucher and non-voucher search results 
search_target_species = function(cfg, entrez_key = get_entrez_key()){
  
  # read the species list for this run
  vpath = version_path(cfg$version, root_path = cfg$root)
  filename = file.path(vpath,
                       sprintf("%s-species.csv.gz", cfg$region))
  species = readr::read_csv(filename, col_types = 'c')  
  
  rentrez::set_entrez_key(entrez_key)

  sets = names(cfg$entrez)
  
  sets = lapply(names(cfg$entrez), 
    function(set){
      s = dplyr::mutate(Voucher = TRUE,
                        SearchTerm = paste0(.data$Species, cfg$entrez[[set]]$search_modifier),
                        Count = NA_integer_,
                        GeneIDs = NA_character_,
                        Fasta = NA_character_) |>
      dplyr::rowwise() |>
      dplyr::group_map(
        function(tbl, key){
          #initial search for each fish on the list
          SearchResult <- rentrez::entrez_search(db = cfg$entrez[[set]]$db, 
                                                 term = tbl$SearchTerm) 
          tbl$Count <- SearchResult$count
          
          tbl$GeneIDs <- toString(SearchResult$ids)
          
          #add fasta files for all geneids that have fastas, 
          #initially still has "\n" in the strings
          if(tbl$Count > 0) {
            tbl$Fasta <- rentrez::entrez_fetch(db = cfg$entrez[[set]]$db, 
                                               id = SearchResult$ids, 
                                               rettype = "fasta", 
                                               retmode = "text")
          } 
          tbl
          
          
        }) |>
        dplyr::bind_rows() |>
        readr::write_csv(file.path(vpath,
                                   sprintf("%s-%s-entrez-search.csv.gz", cfg$region, set)))
      if (cfg$entez[[set]]$dump){
        opath = dir.create(vpath, paste0(set, "-fasta"))
        dump_fasta(s, outpath = opath, separate = cfg$entez[[set]]$dump_separate, 
                   filename = sprintf("%s-%s.fasta", cfg$region, set))
      }
      s
    })
  sets
}

