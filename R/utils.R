
#' Get cell x, y and z coordinates from a btn object
#' 
#' @param btn btn object
#' @param include_faces logical; should face coordinates be included?
#' @return list with cell coordinate 3d arrays
#' @rdname cell_coordinates
#' @method cell_coordinates btn
#' @importFrom RMODFLOW cell_coordinates
#' @export
rmt_cell_coordinates.btn <- function(btn,
                                 include_faces = FALSE) {
  dis <- rmt_convert_btn_to_dis(btn)
  return(RMODFLOW::rmf_cell_coordinates(dis = dis, include_faces = include_faces))
}

#' @describeIn rmt_cell_coordinates.btn Deprecated function name
#' @export
cell_coordinates.btn <- function(...) {
  .Deprecated(new = "rmt_cell_coordinates.btn", old = "cell_coordinates.btn")
  rmt_cell_coordinates.btn(...)
}

#' Get cell dimensions from a btn object
#' 
#' @param btn btn object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @param include_volume logical; should the cell volumes be included?
#' @param include_faces logical; should face areas be included?
#' @return list with cell dimension 3d arrays
#' @rdname cell_dimensions
#' @method cell_dimensions btn
#' @importFrom RMODFLOW cell_dimensions
#' @export
rmt_cell_dimensions.btn <- function(btn,
                                    hed = NULL,
                                    include_volume = FALSE,
                                    include_faces = FALSE) {
  dis <- rmt_convert_btn_to_dis(btn)
  return(RMODFLOW::rmf_cell_dimensions(dis = dis, hed = hed, include_volume = include_volume, include_faces = include_faces))
}

#' @describeIn rmt_cell_dimensions.btn Deprecated function name
#' @export
cell_dimensions.btn <- function(...) {
  .Deprecated(new = "rmt_cell_dimensions.btn", old = "cell_dimensions.btn")
  rmt_cell_dimensions.btn(...)
}

#' Convert RMT3DMS btn to RMODFLOW dis object
#' 
#' @param btn btn object
#' @return dis object
#' @export
rmt_convert_btn_to_dis <- function(btn) {
  dis <- NULL
  dis$nlay <- btn$nlay
  dis$nrow <- btn$nrow
  dis$ncol <- btn$ncol
  dis$nper <- btn$nper
  dis$itmuni <- btn$tunit
  dis$lenuni <- btn$lunit
  dis$laycbd <- rep(0,dis$nlay)
  dis$delr <- btn$delr
  dis$delc <- btn$delc
  dis$top <- btn$htop
  dis$botm <- btn$dz*NA
  dis$botm[,,1] <- btn$htop - btn$dz[,,1]
  if(dis$nlay >1) for(k in 2:dis$nlay) dis$botm[,,k] <- dis$botm[,,k-1]-btn$dz[,,k]
  dis$perlen <- btn$perlen
  dis$nstp <- btn$nstp
  dis$tsmult <- btn$tsmult
  dis$sstr <- NA
  class(dis) <- c('dis','modflow_package')
  return(dis)
}

#' @describeIn rmt_convert_btn_to_dis Deprecated function name
#' @export
convert_btn_to_dis <- function(...) {
  .Deprecated(new = "rmt_convert_btn_to_dis", old = "convert_btn_to_dis")
  rmt_convert_btn_to_dis(...)
}

#' Convert mt3dms coordinates to real world coordinates
#' 
#' @param x mt3dms x coordinate
#' @param y mt3dms y coordinate
#' @param z mt3dms z coordinate
#' @param i mt3dms row number
#' @param j mt3dms column number
#' @param k mt3dms layer number
#' @param prj prj object
#' @param btn btn object
#' @details Provide either xyz or ijk
#' @return data frame with real world x and y coordinates
#' @export
rmt_convert_grid_to_xyz <- function(x = NULL,
                                    y = NULL,
                                    z = NULL,
                                    i = NULL,
                                    j = NULL,
                                    k = NULL,
                                    prj,
                                    btn = NULL) {
  RMODFLOW::rmf_convert_grid_to_xyz(x=x,y=y,z=z,i=i,j=j,k=k,prj=prj,dis=rmt_convert_btn_to_dis(btn))
}

#' @describeIn rmt_convert_grid_to_xyz Deprecated function name
#' @export
convert_btn_to_real <- function(...) {
  .Deprecated(new = "rmt_convert_grid_to_xyz", old = "convert_btn_to_real")
  rmt_convert_grid_to_xyz(...)
}

#' Convert real world coordinates to mt3dms coordinates
#' 
#' @param x real world x coordinate
#' @param y real world y coordinate
#' @param prj prj object
#' @param z real world z coordinate; optional
#' @param btn btn object; optional
#' @details
#' If btn is not provided, only x, y and z coordinates are returned. If z is not provided, no third dimension coordinates are returned.
#' @return data frame with x, y, z, i, j, k, roff, coff and loff modflow coordinates
#' @export
rmt_convert_xyz_to_grid <- function(x,y,prj,z=NULL,btn=NULL) {
  RMODFLOW::rmf_convert_xyz_to_grid(x = x, y = y, prj = prj, z = z, dis = rmt_convert_btn_to_dis(btn))
}

#' @describeIn rmt_convert_xyz_to_grid Deprecated function name
#' @export
convert_real_to_btn <- function(...) {
  .Deprecated(new = "rmt_convert_xyz_to_grid", old = "convert_real_to_btn")
  rmt_convert_xyz_to_grid(...)
}

#' Add rmt3dms array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @param dim the dim attribute for the array to be created; by default, dim(obj) is used
#' @export
rmt_create_array <- function(obj = NA, dim = NULL) {
  array <- RMODFLOW::rmf_create_array(obj = obj, dim = dim)
  class(array) <- convert_rmodflow_to_rmt3dms_class(array)
  return(array)
}

#' @describeIn rmt_create_array Deprecated function name
#' @export
create_rmt3dms_array <- function(...) {
  .Deprecated(new = "rmt_create_array", old = "create_rmt3dms_array")
  rmt_create_array(...)
}
