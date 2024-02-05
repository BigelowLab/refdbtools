#' Compose a version path
#'
#' @export
#' @param version chr, a segemented version ID such as 'v1.234'
#' @param root_path chr, the root path
#' @return a path description
version_path = function(version = "v0.000",
                        root_path = "."){
  vparts = charlier::parse_version(version)
  file.path(root_path[1], "data", "versions", vparts[[1]], version)
}

#' Read a configuration
#'
#' @export
#' @param filename chr, the full name of the file, ignored if version is not NULL
#' @param version chr, a segemented version ID such as 'v1.234' or NULL
#' @param root_path chr, the root path
#' @return a configuration
read_configuration = function(filename,
                              version = NULL,
                              root_path = "."){

  if (!is.null(version)){
    filename = file.path(version_path(version, root_path),
                    sprintf("%s.yaml", version))
  }
  yaml::read_yaml(filename)
}

#' Read a configuration
#'
#' @param cfg list, configuration list
#' @param root_path chr, the root path or NULL in which case the root path is taken
#'   from the config it self
#' @return the input configuration
write_configuration = function(cfg,
                               root_path = NULL){

  if (is.null(root_path)) root_path = cfg$rootpath
  vfile = file.path(version_path(cfg$version, root_path),
                    sprintf("%s.yaml", cfg$version))
  yaml::write_yaml(cfg, vfile)
  cfg
}

#' Retrieve the user's entrez_key from ~/.entrez_key
#'
#' @param filename chr the name of the entrez key filename
#' @return the entrez key
get_entrez_key = function(filename = "~/.entrez_key"){
  readLines(filename)
}
