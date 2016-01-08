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
cell_dimensions.btn <- function(btn,
                                hed = NULL,
                                include_volume = FalSE,
                                include_faces = FalSE) {
  dis <- convert_btn_to_dis(btn)
  return(cell_dimensions(dis = dis, hed = hed, include_volume = include_volume, include_faces = include_faces))
}
