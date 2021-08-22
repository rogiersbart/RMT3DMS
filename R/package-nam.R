
#' Create an \code{RMT3DMS} nam object
#' 
#' \code{rmt_create_nam} creates an \code{RMT3DMS} nam object.
#' 
#' @param ... RMT3DMS objects to be included in the nam file
#' @param ftl path to the flow-transport link file; typically '.ftl'
#' @param ftl_free logical; is the flow-transport link file written in free (formatted) format (TRUE) or binary (unformatted) (FALSE)? if NULL (default), it is guessed from reading \code{ftl}
#' @param print logical; should the contents of the flow-transport link file be printed to the listing file? Defaults to FALSE.
#' @param basename character specifying the basename of the files. The default (\code{NULL}) sets input basenames to 'input' and output to 'output'.
#' @return Object of class mt3d_nam
#' @details If a \code{RMT3DMS nam} object is present, it is recreated. 
#'  It is advised to place the ftl file in the same directory as the transport model input files.
#'  The path the flow-transport link file is normalized in \code{rmt_write_nam}.
#' @export
#' @seealso \code{\link{rmt_read_nam}}, \code{\link{rmt_write_nam}}
#' @examples
#' btn <- rmt_create_btn()
#' adv <- rmt_create_adv()
#' gcg <- rmt_create_gcg()
#' rmt_create_nam(btn, adv, gcg, ftl = 'output.ftl', ftl_free = TRUE)
#' 
rmt_create_nam <- function(..., ftl = {cat('Please select corresponding ftl file ...\n'); file.choose()}, ftl_free = NULL, print = FALSE, basename = NULL) {
  
  fobjects <- list(...)
  if(length(fobjects) == 1 && inherits(fobjects[[1]], c('list', 'mt3dms')) && !('rmt_package' %in% class(fobjects[[1]]))) fobjects <- unclass(fobjects[[1]])
  nam_path <- ''
  
  # data set 1
  df <- rmti_list_packages(type = 'usgs')
  # check if all input are rmt_packages & add all input objects
  all_rmt <- vapply(fobjects, function(i) 'rmt_package' %in% class(i), TRUE)
  if(prod(all_rmt) == 0) stop('Please make sure all objects are RMT3DMS rmt_package objects representing MT3DMS input', call. = FALSE)
  classes <- vapply(fobjects, function(i) class(i)[which(class(i) == 'rmt_package')-1], 'text')
  
  # remove possible NAM object
  if('mt3d_nam' %in% classes) {
    nam_path <- attr(fobjects[[which(classes == 'mt3d_nam')]], 'dir')
    warning('Removing old nam object', call. = FALSE)
    fobjects <- fobjects[-which(classes == 'mt3d_nam')]
    classes <- classes[-which(classes == 'mt3d_nam')]
  }
  
  # FT6
  if('ft6' %in% classes) {
    stop('ft6 not yet supported', call. = FALSE)
    ftl_ftype <- 'FT6'
    ftl_fname # set from ft6 object
  } else {
    ftl_ftype <- 'FTL'
    ftl_fname <- ftl
    if(is.null(ftl_free)) {
      binary <- rmti_check_ftl_binary(ftl)
      if(binary == 'empty') stop('FTL file is empty', call. = FALSE)
    } else {
      binary <- !ftl_free
    }
  }
  
  if(is.null(basename)) {
    input <- 'input'
    output <- 'output'
  } else {
    input <- output <- basename
  }
  
  nam <- data.frame(ftype = c('LIST', ftl_ftype, rep(NA, length(fobjects))),
                    nunit = c(50, 50 + c(1, seq_along(fobjects) + 1)),
                    fname = c(paste(output, 'm3d', sep = '.'), ftl_fname, rep(NA, length(fobjects))),
                    options = c(NA, ifelse(binary, NA, ifelse(print, 'FREE PRINT', 'FREE')), rep(NA, length(fobjects))), stringsAsFactors = FALSE)
  
  for(i in seq_along(fobjects)) {
    nam$fname[i+2] <- paste(input, classes[i], sep = '.')
    nam$ftype[i+2] <- df$ftype[classes[i] == df$rmt]
  }
  
  # check for additional output files
  if('TOB' %in% nam$ftype) {
    tob <- fobjects[[which(nam$ftype=='TOB')-2]]
    if(tob$inconcobs > 0) {
      if(tob$inconcobs %in% nam$nunit) stop('tob$inconcobs unit number already in use by package ', nam$ftype[which(nam$nunit == tob$inconcobs)],
                                            '. Please set tob$inconcobs to an unused unit number.', call. = FALSE)
    }
    if(tob$influxobs > 0) {
      if(tob$influxobs %in% nam$nunit) stop('tob$influxobs unit number already in use by package ', nam$ftype[which(nam$nunit == tob$influxobs)],
                                            '. Please set tob$influxobs to an unused unit number.', call. = FALSE)
    }
    if(tob$insaveobs > 0) {
      if(tob$insaveobs %in% nam$nunit) stop('tob$insaveobs unit number already in use by package ', nam$ftype[which(nam$nunit == tob$insaveobs)],
                                            '. Please set tob$insaveobs to an unused unit number.', call. = FALSE)
    }
  }
  
  # TODO set user-defined output names if necessary
  
  
  attr(nam, 'dir') <- nam_path
  class(nam) <- c('mt3d_nam','rmt_package','data.frame')
  return(nam)
}

#' Read a MT3DMS name file
#' 
#' \code{rmt_read_nam} reads in a MT3DMS name file and returns it as an \code{\link{RMT3DMS}} nam object.
#' 
#' @param file filename; typically '*.mt_nam'
#' @return object of class mt3d_nam
#' @export
#' @seealso \code{\link{rmt_create _nam}}, \code{\link{rmt_write_nam}}
#' @examples
rmt_read_nam <- function(file = {cat('Please select nam file ...\n'); file.choose()}) {
  
  nam <- list()
  lines <- readr::read_lines(file)
  
  # top comments
  data_set_0 <- rmti_parse_comments(lines)
  comments <- data_set_0$comments
  lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  indices <- rep(TRUE,length(lines))
  for(i in 1:length(lines)) {
    if(length(rmti_remove_empty_strings(strsplit(lines[i],' |\t')[[1]])) == 0 || strsplit(rmti_remove_empty_strings(strsplit(lines[i],' |\t')[[1]])[1], "")[[1]][1] == "#") {
      comments <-  c(comments, gsub('#', '', lines[i]))
      indices[i] <- FALSE
    } else {
      lines[i] <- rmti_remove_comments_end_of_line(lines[i])
    }
  }
  
  nam_lines <- lines[indices]
  nam_lines <- lapply(strsplit(nam_lines, ' |\t'), rmti_remove_empty_strings)
  nam_lines <- lapply(nam_lines, function(i) rmti_ifelse0(length(unlist(i)) < 4, c(unlist(i),NA), unlist(i)))
  nam_lines <- lapply(nam_lines, function(i) rmti_ifelse0(length(i) > 4, c(i[1:3], paste(i[4:length(i)], collapse = ' ')), i))
  
  nam <- data.frame(do.call(rbind, nam_lines), stringsAsFactors = FALSE)
  colnames(nam) <- c('ftype','nunit','fname', 'options')
  nam$nunit<- as.numeric(nam$nunit)
  nam$fname <- gsub('\"|\'', '', nam$fname)
  nam$ftype <- toupper(nam$ftype)
  
  spaces_in_fname <- !is.na(nam[[4]]) & !grepl(paste(c('OLD', 'REPLACE', 'UNKNOWN', 'FREE', 'PRINT', 'NOPRINT'), collapse = '|'), toupper(nam[[4]]))
  if(any(spaces_in_fname)) warning('nam$option should either be FREE and/or PRINT for the FTL file. This warning might be generated due to whitespaces in fname which are not allowed ',
                                   '(records ', paste(which(spaces_in_fname), collapse = ', '), ')', call. = FALSE)
  
  if(any(c(1:23) %in% nam$nunit) || any(nam$unit > 100)) warning('nunit 1-23 or > 100 detected. These unit numbers are not allowed by MT3DMS.', call. = FALSE)
  
  comment(nam) <- comments
  attr(nam, 'dir') <- dirname(file)
  class(nam) <- c('mt3d_nam', 'rmt_package', 'data.frame')
  return(nam)
}

#' Write a MT3DMS name file
#' 
#' \code{rmt_write_nam} writes a MT3DMS name file based on an \code{\link{RMT3DMS}} nam object.
#' 
#' @param nam an \code{\link{RMT3DMS}} nam object
#' @param file filename to write to; typically '*.mt_nam'
#' @param exclude character vector with packages names to exclude from the simulation. Defaults to NULL
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmt_read_nam}}, \code{\link{rmt_create_nam}}
#' @examples 
#' btn <- rmt_create_btn()
#' adv <- rmt_create_adv()
#' dsp <- rmt_create_dsp(btn = btn)
#' gcg <- rmt_create_gcg()
#' nam <- rmt_create_nam(btn, adv, gcg, btn, dsp, ftl = 'output.ftl', ftl_free = FALSE)
#' f <- tempfile()
#' rmt_write_nam(nam, file = f, exclude = 'dsp')
#' 
rmt_write_nam <- function(nam,
                          file = {cat('Please select nam file to overwrite or provide new filename ...\n'); file.choose()},
                          exclude = NULL) {
  
  if(!is.null(exclude)) {
    df <- rmti_list_packages(type = 'usgs')
    ftype <- df$ftype[which(df$rmt %in% exclude)]
    nam <- nam[-which(nam$ftype %in% ftype), ]
  }
  if(any(duplicated(nam$nunit[which(nam$nunit != 0)]))) stop('Please make sure every file has a unique or 0 nunit specified', call. = FALSE)
  if(any(c(1:23) %in% nam$nunit) || any(nam$unit > 100)) warning('nunit 1-23 or > 100 detected. These unit numbers are not allowed by MT3DMS.', call. = FALSE)
  nam$nunit <- as.integer(nam$nunit)
  
  # try to normalize ftl
  if(dirname(file) == dirname(nam$fname[which(nam$ftype == 'FTL')])) {
    nam$fname[which(nam$ftype == 'FTL')] <- basename(nam$fname[which(nam$ftype == 'FTL')])
  }
  
  # check for spaces in fname
  if(any(grepl(' |\t', nam$fname))) stop('Whitespaces are not allowed in fname', call. = FALSE)
  spaces_in_fname <- !is.na(nam[[4]]) & !grepl(paste(c('OLD', 'REPLACE', 'UNKNOWN', 'FREE', 'PRINT', 'NOPRINT'), collapse = '|'), toupper(nam[[4]]))
  if(any(spaces_in_fname)) warning('nam$option should either be FREE or PRINT for the FTL file. This warning might be generated due to whitespaces in fname which are not allowed ',
                                   '(records ', paste(which(spaces_in_fname), collapse = ', '), ')', call. = FALSE)
  
  # data set 0
  v <- packageDescription("RMT3DMS")$Version
  cat(paste('# MT3DMS Name File created by RMT3DMS, version',v,'\n'), file=file)
  cat(paste('#', comment(nam)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  # MT3DMS does not support tab-delimited values in NAM file
  # write.table(nam, file = file, row.names = FALSE, col.names = FALSE, quote = FALSE, na = '', append = TRUE, sep = ' ')
  readr::write_delim(nam, file = file, append = TRUE, col_names = FALSE, quote = 'none', escape = 'none', na = '', delim = ' ')
}
