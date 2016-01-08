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
convert_real_to_btn <- function(x,y,prj,z=NULL,btn=NULL) {
  RMODFLOW::convert_real_to_dis(x = x, y = y, prj = prj, z = z, dis = convert_btn_to_dis(btn))
}
