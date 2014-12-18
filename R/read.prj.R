#' Read a projection file
#' 
#' \code{read.prj} reads in projection file and returns it as an \code{\link{RMT3DMS}} prj object.
#' 
#' @param file Filename; typically *.prj
#' @return Object of class prj
#' @export
read.prj <- function(file)
{
  prj.lines <- scan(file, what=character(), sep='\n')
  prj <- NULL
  prj$projection <- prj.lines[1]
  prj$origin <- as.numeric(remove.empty.strings(strsplit(prj.lines[2],' ')[[1]]))
  prj$rotation <- as.numeric(remove.empty.strings(strsplit(prj.lines[3],' ')[[1]])[1])
  class(prj) <- 'prj'
  return(prj)
}