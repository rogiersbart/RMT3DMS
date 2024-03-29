#' Path to example file
#'
#' @param filename filename of the example file. If NULL (default), returns vector of all example file filenames.
#'
#' @return path to example file
#' @export
#'
#' @examples
rmt_example_file <- function(filename = NULL) {
  if(is.null(filename)) {
    dir(system.file("extdata", package = "RMT3DMS"))
  } else {
    return(system.file(paste0("extdata/", filename), package = "RMT3DMS")  )
  }
}