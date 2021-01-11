
#' Create a \code{RMT3DMS} tob object
#' 
#' \code{rmt_create_tob} creates an RMT3DMS tob object
#'
#' @param locations data.frame or \code{sf POINT} object with the name, x and y coordinates (if data.frame), and top and bottom of the monitoring well filter
#' @param time_series data.frame with the name of the monitoring well filter, icomp, weight and the observation time and concentration
#' @param btn \code{RMT3DMS} btn object
#' @param outnam character specifying the basename of the output files. Defaults to 'output'.
#' @param save_binary logical specifying if the calculated concentrations and mass fluxes should be saved to an unformatted output file {outnam}.PST. Defaults to FALSE. 
#' @param cscale scaling factor for the observed concentrations. Defaults to 1.
#' @param ioutcobs logical specifying if the residual errors between calculated and observed concentrations should be written to the output file. Defaults to TRUE.
#' @param iconclog logical specifying if the calculated and observed concentrations should be converted to the common logarithm (log10) before computing statistics. Defaults to FALSE.
#' @param iconcintp logical indicating if the calculated concentrations at observation locations should be interpolated (bilinear) from neighboring nodal points. Defaults to TRUE.
#' @param mlayw 3d array containing weights for the proportioning of multilayer concentration observations. Defaults to 1 for all cells.
#' @param fscale scaling factor for the observed mass fluxes. Defaults to 1.
#' @param ioutflux logical indicating if the residual errors between calculated and observed mass fluxes should be written to the output file. Defaults to TRUE. 
#' @param fluxobs data.frame with name, icomp, time, weight, flux, group & iss columns. The group specifies the FluxGroup which the observation is part of. iss specifies the sink/source type and should be unique for each group.
#' @param fluxcells data.frame with k, i, j, factor and group columns. The group specifies the FluxGroup which the observation is part of and is used to link to corresponding groups in \code{fluxobs}.
#' @param unique_obsnam logical; should an ID number be added to obsnam for filters with concentration observations at different times prefixed with an underscore? Defaults to FALSE.
#' @param prj \code{RMODFLOW} prj object; provide if coordinates in \code{locations} are real world coordinates
#' @param inconcobs integer specifying the unit number to save the OCN output file to when \code{locations & time_series} are defined. Defaults to 61. 
#' @param influxobs integer specifying the unit number to save the MFX output file to when \code{fluxobs & fluxcells} are defined. Defaults to 62.
#' @param insaveobs integer specifying the unit number to save the binary PST output file to when \code{locations & time_series} and/or \code{fluxobs & fluxcells} are defined and when \code{save_binary = TRUE}. Defaults to 63.
#' 
#' The \code{locations} data.frame must have columns named \code{name, x, y, top, bottom}. Names should be unique.
#' The \code{time_series} data.frame must have columns named \code{name, icomp, weight, time, concentration}.
#' 
#' The \code{fluxobs} data.frame must have columns named \code{name, icomp, weight, time, flux, group, iss}.
#' The \code{fluxcells} data.frame must have columns named \code{k, i, j, factor, group}.
#' 
#' @return Object of class \code{tob}
#' @export
#' @seealso \code{\link{rmt_read_tob}}, \code{\link{rmt_write_tob}}
#' @examples
#' 
#' btn <- rmt_create_btn()
#' 
#' locations <- data.frame(x = c(766, 123), y = c(880, 245), top = -5, bottom = c(-6, -16), name = c('well.a', 'well.b'))
#' time_series <- data.frame(time = c(1, 0.2, 1), icomp = 1, weight = 1, name = c('well.a', 'well.b', 'well.b'), concentration = c(25, 30, 35))
#'
#' 
#' fluxobs <- data.frame(name = c('chd', 'riv', 'riv'), icomp = 1, time = c(-1, 0.5, 1),  weight = 1,
#'                       flux = c(500, -22.5, -33.2), group = c(1, 2, 2), iss = c(1, 4, 4))
#' fluxcells <- data.frame(k = 1, i = c(1, 10, 10, 10, 10), j = c(1, 7, 8, 9, 10),
#'                         factor = 1, group = c(1, 2, 2, 2, 2))
#' 
#' rmt_create_tob(locations, time_series, btn, fluxobs = fluxobs, fluxcells = fluxcells, unique_obsnam = TRUE)
#' 
rmt_create_tob <- function(locations = NULL,
                           time_series =  NULL,
                           btn,
                           outnam = 'output',
                           save_binary = FALSE,
                           cscale = 1,
                           ioutcobs = TRUE,
                           iconclog = FALSE,
                           iconcintp = TRUE,
                           mlayw = rmt_create_array(1, dim = c(btn$nrow, btn$ncol, btn$nlay)),
                           fscale = 1,
                           ioutflux = TRUE,
                           fluxobs = NULL,
                           fluxcells = NULL,
                           unique_obsnam = FALSE,
                           prj = RMODFLOW::rmf_get_prj(btn),
                           inconcobs = 61,
                           influxobs = 62,
                           insaveobs = 63) {
  
  if(is.null(locations) && is.null(time_series) && is.null(fluxobs) && is.null(fluxcells)) {
    stop('Please specify locations & time_series and/or fluxobs & fluxcells', call. = FALSE)
  }
  if(sum(is.null(locations), is.null(time_series)) == 1) stop('locations & time_series should both be specified or neither', call. = FALSE)
  if(sum(is.null(fluxobs), is.null(fluxcells)) == 1) stop('fluxobs & fluxcells should both be specified or neither', call. = FALSE)
  
  tob <- list()
  dis <- rmt_convert_btn_to_dis(btn, prj = prj)
  
  iunit.ocn <- inconcobs
  iunit.mfx <- influxobs
  iunit.pst <- insaveobs
  
  # data set 0
  # comments should be provided with ?comment
  
  # data set 1
  tob$maxconcobs <- 0
  tob$maxfluxobs <- 0
  tob$maxfluxcells <- 0
  
  # data set 2
  tob$outnam <- outnam
  tob$inconcobs <- 0
  tob$influxobs <- 0
  tob$insaveobs <- ifelse(save_binary, iunit.pst, 0)
  
  # concentrations
  if(!is.null(locations) && !is.null(time_series)) {
    
    # locations is sf object
    if(inherits(locations, 'sf')) {
      geom <- unique(sf::st_geometry_type(locations))
      if(length(geom) > 1 || geom != 'POINT') stop('A locations sf object should have geometry type POINT for all features', call. = FALSE)
      if(!is.na(sf::st_crs(locations))) {
        if(is.null(prj) || sf::st_crs(locations) != sf::st_crs(prj$crs)) stop('When locations is an sf object with a crs, prj should be provided with the same crs', call. = FALSE)
      } else {
        if(!is.null(prj) && !is.na(sf::st_crs(prj$crs))) stop('prj has a crs defined whereas locations does not', call. = FALSE) 
      }
      coords <- setNames(as.data.frame(sf::st_coordinates(locations)), c('x', 'y'))
      locations <- cbind(sf::st_set_geometry(locations, NULL), coords)
    }
    
    # error checks in locations & time_series
    if(!all(c('x', 'y', 'name', 'top', 'bottom') %in% colnames(locations))) stop('locations object must have columns x, y, name, top and bottom', call. = FALSE)
    if(!all(c('name', 'icomp', 'weight', 'time', 'concentration') %in% colnames(time_series))) stop('time_series data.frame must have columns name, icomp, weight, time and concentration', call. = FALSE)
    if(any(duplicated(locations$name))) stop('locations should not have duplicated names', call. = FALSE)
    
    # set locations tops and bottoms
    locations_top <- cbind(locations[,c('x','y','top','name')], suppressWarnings(RMODFLOW::rmf_convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$top, dis = dis, prj = prj, output = c('ijk', 'off'))))
    locations_bottom <- cbind(locations[,c('x','y','bottom','name')], suppressWarnings(RMODFLOW::rmf_convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$bottom, dis = dis, prj = prj, output = c('ijk', 'off'))))
    if(any(is.na(locations_top)) || any(is.na(locations_bottom))) {
      na_id <- unique(c(which(is.na(locations_top), arr.ind = TRUE)[, 1], which(is.na(locations_bottom), arr.ind = TRUE)[, 1]))
      locations_top <- locations_top[-na_id, ]
      locations_bottom <- locations_bottom[-na_id, ]
      na_names <- locations$name[na_id]
      locations <- locations[-na_id, ]
      time_series <- subset(time_series, !(name %in% na_names))
      warning('Removing observations outside the grid domain: ', paste(na_names, collapse = ' '), call. = FALSE)
      if(nrow(locations_top) == 0 || nrow(locations_bottom) == 0 || nrow(locations) == 0) stop('No observations inside grid domain', call. = FALSE)
    }
    if(!setequal(locations$name, time_series$name)) {
      na_names <- setdiff(union(locations$name, time_series$name), intersect(locations$name, time_series$name))
      warning('Following observations are not present in both locations and time series data.frames and are therefore removed: ', 
              paste(na_names, collapse= ' '), call. = FALSE)
      locations <- subset(locations, !(name %in% na_names))
      time_series <- subset(time_series, !(name %in% na_names))
      locations_top <- subset(locations_top, !(name %in% na_names))
      locations_bottom <- subset(locations_bottom, !(name %in% na_names))
      if(nrow(time_series) == 0) stop('time series is empty', call. = FALSE)
      if(nrow(locations) == 0) stop('locations is empty', call. = FALSE)
    }
    
    locations_top$k <- ifelse(locations_top$loff == 0.5, locations_top$k + 1, locations_top$k)
    locations_bottom$k <- ifelse(locations_bottom$loff == -0.5, locations_bottom$k - 1, locations_bottom$k)  
    
    # tob
    
    # data set 0
    # comments should be provided with ?comment
    
    # data set 1 & 2
    tob$maxconcobs <- nrow(time_series)
    tob$inconcobs <- iunit.ocn
    
    # data set 3
    tob$nconcobs <- nrow(time_series)
    tob$cscale <- cscale
    tob$ioutcobs <- as.numeric(ioutcobs)
    tob$iconclog <- as.numeric(iconclog)
    tob$iconcintp <- as.numeric(iconcintp)
    
    # data set 4-5
    df <- list()
    df$cobsnam <- df$layer <- df$row <- df$column <- df$timeobs <- df$roff <- df$coff <- df$weight <- df$cobs <- rep(NA, tob$nconcobs)
    df$mlay <- df$pr <- list()
    
    if(any(nchar(locations$name) > 12) | any(grepl('\\s+', locations$name))) {
      stop('Concentration observation names should be maximum 12 characters and not contain spaces.', call. = FALSE)
    }
    
    for(i in 1:nrow(locations)) {
      # locations
      ts_id <- which(time_series$name == locations$name[i])
      df$row[ts_id] <- locations_top$i[i]
      df$column[ts_id] <- locations_top$j[i]
      
      df$roff[ts_id] <- locations_top$roff[i]
      df$coff[ts_id] <- locations_top$coff[i]
      if(locations_top$k[i] == locations_bottom$k[i]) {
        df$layer[ts_id] <- locations_top$k[i]
        m_lay <- 1
      } else {
        m_lay <- - (locations_bottom$k[i] - locations_top$k[i] + 1)
        df$layer[ts_id] <- m_lay
      }
      
      # multiple layers
      # TODO set proper pr
      df$mlay[ts_id] <- df$pr[ts_id] <- NA
      if(m_lay < 0) {
        df$mlay[ts_id] <- lapply(df$mlay[ts_id], function(x) x <- locations_top$k[i]:locations_bottom$k[i]) 
        length_in_cell <- rep(NA, abs(df$layer[ts_id[1]]))
        for(j in 1:abs(df$layer[ts_id[1]])) {
          m_val <- df$mlay[[ts_id[1]]][j]
          layer_tops <- RMODFLOW::rmf_cell_coordinates(dis, include_faces = TRUE)$upper
          length_in_cell[j] <- min(locations$top[i], layer_tops[locations_top$i[i],locations_top$j[i], m_val]) - max(locations$bottom[i], dis$botm[locations_top$i[i],locations_top$j[i], m_val])
        }
        df$pr[ts_id] <- lapply(df$pr[ts_id], function(x) x <- mlayw[locations_top$i[i],locations_top$j[i],df$mlay[[ts_id[1]]]] * length_in_cell) 
        df$pr[ts_id] <- lapply(df$pr[ts_id], function(x) x <- df$pr[[ts_id[1]]]/sum(df$pr[[ts_id[1]]]))
      } 
      
      # time series
      if(unique_obsnam) {
        df$cobsnam[ts_id] <- paste(locations$name[i], c(1:length(ts_id)), sep = '_')
      } else {
        df$cobsnam[ts_id] <- rep(as.character(locations$name[i]), length(ts_id))
      }   
      
      df$timeobs[ts_id] <- time_series$time[ts_id]
      df$icomp[ts_id] <- time_series$icomp[ts_id]
      df$weight[ts_id] <- time_series$weight[ts_id]
      df$cobs[ts_id] <- time_series$concentration[ts_id]
    }
    
    if(!any(df$layer < 0)) {
      df$mlay <- df$layer
      df$pr <- rep(1, tob$nconcobs)
    } else {
      s_lay <- which(df$layer > 0)
      df$mlay[s_lay] <- df$layer[s_lay]
      df$pr[s_lay] <- 1
    }
    
    if(any(nchar(df$cobsnam) > 12)) stop('Concentration observation names should be maximum 12 characters. This may be caused by unique_obsnam = TRUE', call. = FALSE)
    
    # Data
    tob$concentrations <- data.frame(name = df$cobsnam, layer = I(df$mlay), pr = I(df$pr), row = df$row, column = df$column, icomp = df$icomp, 
                                     timeobs = df$timeobs, roff = df$roff, coff = df$coff, weight = df$weight, cobs = df$cobs, stringsAsFactors = FALSE)
  }
  
  # mass fluxes
  if(!is.null(fluxobs) && !is.null(fluxcells)) {

    # error checks in locations & time_series
    if(!all(c('name', 'icomp', 'weight', 'time', 'flux', 'group', 'iss') %in% colnames(fluxobs))) stop('fluxobs object must have columns name, icomp, weight, time, flux, group and iss', call. = FALSE)
    if(!all(c('k', 'i', 'j', 'factor', 'group') %in% colnames(fluxcells))) stop('fluxcells data.frame must have columns k, i, j, factor and group', call. = FALSE)
    
    # check for non-corresponding group names
    if(!setequal(fluxobs$group, fluxcells$group)) {
      na_names <- setdiff(union(fluxobs$group, fluxcells$group), intersect(fluxobs$group, fluxcells$group))
      warning('Following groups are not present in both fluxobs and fluxcells data.frames and are therefore removed: ', 
              paste(na_names, collapse= ' '), call. = FALSE)
      fluxobs <- subset(fluxobs, !(group %in% na_names))
      fluxcells <- subset(fluxcells, !(group %in% na_names))
      if(nrow(fluxcells) == 0) stop('fluxcells is empty', call. = FALSE)
      if(nrow(fluxobs) == 0) stop('fluxobs is empty', call. = FALSE)
    }
    
    # data set 1 & 2
    tob$maxfluxobs <- nrow(fluxobs)
    tob$maxfluxcells <- nrow(fluxcells)
    tob$influxobs <- iunit.ocn
    
    # data set 6
    group_names <- unique(fluxobs$group)
    
    tob$nfluxgroup <- length(group_names)
    tob$fscale <- fscale
    tob$ioutflux <- as.numeric(ioutflux)
     
    # data set 7-9
    tob$isstype <- tob$ncells <- tob$nfluxtimeobs <- rep(NA, tob$nfluxgroup)
    
    tob$fluxobs <- fluxobs
    tob$fluxcells <- fluxcells
    
    for(i in 1:tob$nfluxgroup) {
      obs_ids <- which(as.character(tob$fluxobs$group) == as.character(group_names[i]))
      cell_ids <- which(as.character(tob$fluxcells$group) == as.character(group_names[i]))
      
      tob$nfluxtimeobs[i] <- length(obs_ids)
      tob$ncells[i] <- length(cell_ids)
      iss <- unique(tob$fluxobs$iss[obs_ids])
      if(length(iss) > 1) warning('iss should be unique per group. Using the first value', call. = FALSE)
      tob$isstype[i] <- iss[1]
      
      if(unique_obsnam) {
        tob$fluxobs$name[obs_ids] <- paste(tob$fluxobs$name[obs_ids], c(1:length(obs_ids)), sep = '_')
      }
    }
    
    if(any(nchar(tob$fluxobs$name) > 12) | any(grepl('\\s+', tob$fluxobs$name))) {
      stop('Flux observation names should be maximum 12 characters and only contain non-blank characters. This may be caused by unique_obsnam = TRUE', call. = FALSE)
    }
    
    # names(tob$fluxobs)[which(names(tob$fluxobs) == 'name')] <- 'fobsnam'
    # names(tob$fluxobs)[which(names(tob$fluxobs) == 'flux')] <- 'fluxobs'
    # names(tob$fluxobs)[which(names(tob$fluxobs) == 'time')] <- 'fluxtimeobs'
    # names(tob$fluxobs)[which(names(tob$fluxobs) == 'weight')] <- 'weight_fobs'
  }
  
  if(tob$inconcobs == 0 && tob$influxobs == 0) tob$insaveobs <- 0
  class(tob) <- c('tob', 'rmt_package')
  return(tob)
}


#' Read an MT3DMS transport observation package file 
#'
#' \code{rmt_read_tob} reads in an MT3DMS transport observation package file and returns it as an \code{\link{RMT3DMS}} tob object.
#'
#' @param file filename; typically '*.tob'
#'
#' @return object of class tob
#' @export
#' @seealso \code{\link{rmt_create_tob}}, \code{\link{rmt_write_tob}}
rmt_read_tob <- function(file = {cat('Please select tob file ...\n'); file.choose()}) {
  
  tob_lines <- readr::read_lines(file)
  tob <- list()
  
  # Data set 0
  data_set_0 <- rmti_parse_comments(tob_lines)
  comment(tob) <- data_set_0$comments
  tob_lines <- data_set_0$remaining_lines
  rm(data_set_0)  
  
  # data set 1
  data_set_1 <- rmti_parse_variables(tob_lines, n = 3, format = 'free')
  tob$maxconcobs <- as.numeric(data_set_1$variables[1])
  tob$maxfluxobs <- as.numeric(data_set_1$variables[2])
  tob$maxfluxcells <- as.numeric(data_set_1$variables[3])
  tob_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 <- rmti_parse_variables(tob_lines, n = 4, format = 'free', character = TRUE)
  tob$outnam <- as.character(data_set_2$variables[1])
  tob$inconcobs <- as.numeric(data_set_2$variables[2])
  tob$influxobs <- as.numeric(data_set_2$variables[3])
  tob$insaveobs <- as.numeric(data_set_2$variables[4])
  tob_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  
  # concentrations
  if(tob$inconcobs > 0) {
    
    # data set 3
    data_set_3 <- rmti_parse_variables(tob_lines, n = 5, format = 'free')
    tob$nconcobs <- as.numeric(data_set_3$variables[1])
    tob$cscale <- as.numeric(data_set_3$variables[2])
    tob$ioutcobs <- as.numeric(data_set_3$variables[3]) > 0
    tob$iconclog <- as.numeric(data_set_3$variables[4]) > 0
    tob$iconcintp <- as.numeric(data_set_3$variables[5]) > 0
    tob_lines <- data_set_3$remaining_lines
    rm(data_set_3)
    
    df <- list()
    df$cobsnam <-  df$layer <- df$row <- df$column <- df$icomp <- df$timeobs <- df$weight <- df$roff <- df$coff <- df$cobs <- rep(NA, tob$nconcobs)
    df$mlay <- df$pr <- list()
    
    for(i in 1:tob$nconcobs) {
      # data set 4
      data_set_4 <- rmti_parse_variables(tob_lines, n = 10, format = 'free', character = TRUE)
      df$cobsnam[i] <- as.character(data_set_4$variables[1])
      df$layer[i] <- as.numeric(data_set_4$variables[2])
      df$row[i] <- as.numeric(data_set_4$variables[3])
      df$column[i] <- as.numeric(data_set_4$variables[4])
      df$icomp[i] <- as.numeric(data_set_4$variables[5])
      df$timeobs[i] <- as.numeric(data_set_4$variables[6])
      df$roff[i] <- as.numeric(data_set_4$variables[7])
      df$coff[i] <- as.numeric(data_set_4$variables[8])
      df$weight[i] <- as.numeric(data_set_4$variables[9])
      df$cobs[i] <- as.numeric(data_set_4$variables[10])
      tob_lines <- data_set_4$remaining_lines
      rm(data_set_4)
      
      # data set 5
      if(df$layer[i] < 0) {
        mlay <- pr <- vector(mode = 'numeric', length = abs(df$layer[i]))
        data_set_5 <- rmti_parse_variables(tob_lines, n = 2*(abs(df$layer[i])), format = 'free')
        for(layerNr in 1:abs(df$layer[i])) {
          mlay[layerNr] <- data_set_5$variables[(2*layerNr)-1]
          pr[layerNr] <- data_set_5$variables[2*layerNr]
        }
        df$mlay[[i]] <- mlay[1:abs(df$layer[i])]
        df$pr[[i]] <- pr[1:abs(df$layer[i])]
        tob_lines <- data_set_5$remaining_lines
        rm(data_set_5)
      }
    }
    
    if(!any(df$layer < 0)) {
      df$mlay <- df$layer
      df$pr <- rep(1, tob$nconcobs)
    } else {
      s_lay <- which(df$layer > 0)
      df$mlay[s_lay] <- df$layer[s_lay]
      df$pr[s_lay] <- 1
    }
    
    # Data
    tob$concentrations <- data.frame(name = df$cobsnam, layer = I(df$mlay), pr = I(df$pr), row = df$row, column = df$column, icomp = df$icomp, 
                                     timeobs = df$timeobs, roff = df$roff, coff = df$coff, weight = df$weight, cobs = df$cobs, stringsAsFactors = FALSE)
    
  }
  
  # mass fluxes
  if(tob$influxobs > 0) {
    
    # data set 6
    data_set_6 <- rmti_parse_variables(tob_lines, n = 3, format = 'free')
    tob$nfluxgroup <- as.numeric(data_set_6$variables[1])
    tob$fscale <- as.numeric(data_set_6$variables[2])
    tob$ioutflux <- as.numeric(data_set_6$variables[3]) > 0
    tob_lines <- data_set_6$remaining_lines
    rm(data_set_6)
    
    tob$isstype <- tob$ncells <- tob$nfluxtimeobs <- rep(NA, tob$nfluxgroup)
    fluxobs <- list()
    fluxcells <- list()
    
    for(i in 1:tob$nfluxgroup) {
      # data set 7
      data_set_7 <- rmti_parse_variables(tob_lines, n = 3, format = 'free')
      tob$nfluxtimeobs[i] <- as.numeric(data_set_7$variables[1])
      tob$ncells[i] <- as.numeric(data_set_7$variables[2])
      tob$isstype[i] <- as.numeric(data_set_7$variables[3])
      tob_lines <- data_set_7$remaining_lines
      rm(data_set_7)
      
      dfobs <- data.frame(name = rep(NA_character_, tob$nfluxtimeobs[i]), icomp = rep(NA, tob$nfluxtimeobs[i]), 
                          time = rep(NA, tob$nfluxtimeobs[i]), weight = rep(NA, tob$nfluxtimeobs[i]), flux = rep(NA, tob$nfluxtimeobs[i]),
                          group = rep(i, tob$nfluxtimeobs[i]), iss = rep(tob$isstype[i], tob$nfluxtimeobs[i]), stringsAsFactors = FALSE)
      
      # data set 8
      for(j in 1:tob$nfluxtimeobs[i]) {
        data_set_8 <- rmti_parse_variables(tob_lines, n = 5, format = 'free')
        dfobs$name[j] <- as.character(data_set_8$variables[1])
        dfobs$icomp[j] <- as.numeric(data_set_8$variables[2])
        dfobs$time[j] <- as.numeric(data_set_8$variables[3])
        dfobs$weight[j] <- as.numeric(data_set_8$variables[4])
        dfobs$flux[j] <- as.numeric(data_set_8$variables[5])
        tob_lines <- data_set_8$remaining_lines
        rm(data_set_8)
      }
      fluxobs[[i]] <- dfobs
      
      dfcells <- data.frame(k = rep(NA, tob$ncells[i]), i = rep(NA, tob$ncells[i]), j = rep(NA, tob$ncells[i]), 
                          factor = rep(NA, tob$ncells[i]), group = rep(i, tob$ncells[i]), stringsAsFactors = FALSE)
      # data set 9
      for(jj in 1:tob$ncells[i]) {
        data_set_9 <- rmti_parse_variables(tob_lines, n = 4, format = 'free')
        dfcells$k[jj] <- as.numeric(data_set_9$variables[1])
        dfcells$i[jj] <- as.numeric(data_set_9$variables[2])
        dfcells$j[jj] <- as.numeric(data_set_9$variables[3])
        dfcells$factor[jj] <- as.numeric(data_set_9$variables[4])
        tob_lines <- data_set_9$remaining_lines
        rm(data_set_9)
      }
      fluxcells[[i]] <- dfcells
    }
    
    if(length(fluxobs) > 1) tob$fluxobs <- do.call(rbind, fluxobs)
    if(length(fluxcells) > 1) tob$fluxcells <- do.call(rbind, fluxcells)
  }
  
  class(tob) <- c('tob', 'rmt_package')
  return(tob)
}

#' Write a MT3DMS transport observation package file
#' 
#' @param tob an \code{RMT3DMS} tob object
#' @param file filename to write to; typically '*.tob'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmt_read_tob}}, \code{\link{rmt_create_tob}}
rmt_write_tob <- function(tob, file = {cat('Please select tob file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # Data set 0
  v <- packageDescription("RMT3DMS")$Version
  cat(paste('# MT3DMS Transport Observation File created by RMT3DMS, version',v,'\n'), file=file)
  cat(paste('#', comment(tob)), '\n', file=file, append=TRUE)
  
  # data set 1
  rmti_write_variables(tob$maxconcobs, tob$maxfluxobs, tob$maxfluxcells, file = file, format = 'free')
  
  # data set 2
  rmti_write_variables(tob$outnam, tob$inconcobs, tob$influxobs, tob$insaveobs, file = file, format = 'free')
  
  if(tob$inconcobs > 0) {
    # data set 3
    rmti_write_variables(tob$nconcobs, tob$cscale, as.numeric(tob$ioutcobs), as.numeric(tob$iconclog), as.numeric(tob$iconcintp), file = file, format = 'free')
    
    for(i in 1:tob$nconcobs) {
      # data set 4
      rmti_write_variables(tob$concentrations$name[i], ifelse(length(tob$concentrations$layer[[i]]) > 1, -length(tob$concentrations$layer[[i]]), tob$concentrations$layer[[i]]), 
                           tob$concentrations$row[i], tob$concentrations$column[i], tob$concentrations$icomp[i], 
                           tob$concentrations$timeobs[i], tob$concentrations$roff[i], tob$concentrations$coff[i], tob$concentrations$weight[i],
                           tob$concentrations$cobs[i], file = file, format = 'free')
      
      # data set 5
      if(length(tob$concentrations$layer[[i]]) > 1) {
        rmti_write_variables(paste(tob$concentrations$layer[[i]], tob$concentrations$pr[[i]], collapse = ' '), file = file, format = 'free')
      }
    }
  }
  
  if(tob$influxobs > 0) {
    # data set 6
    rmti_write_variables(tob$nfluxgroup, tob$fscale, as.numeric(tob$ioutflux), file = file, format = 'free')
    
    for(i in 1:tob$nfluxgroup) {
      # data set 7
      rmti_write_variables(tob$nfluxtimeobs[i], tob$ncells[i], tob$isstype[i], file = file, format = 'free')
      
      group <- unique(tob$fluxobs$group)[i]
      fluxobs <- tob$fluxobs[which(tob$fluxobs$group == group),]
      fluxcells <- tob$fluxcells[which(tob$fluxcells$group == group),]
      
      # data set 8
      for(jj in 1:tob$nfluxtimeobs[i]) {
        rmti_write_variables(fluxobs$name[jj], fluxobs$icomp[jj], fluxobs$time[jj], fluxobs$weight[jj], fluxobs$flux[jj], file = file, format = 'free')
      }
      
      # data set 9
      for(jj in 1:tob$ncells[i]) {
        rmti_write_variables(fluxcells$k[jj], fluxcells$i[jj], fluxcells$j[jj], fluxcells$factor[jj], file = file, format = 'free')
      }
    }
  }
  
}
