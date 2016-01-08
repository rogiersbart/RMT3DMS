#' Get cell x, y and z coordinates from a btn object
#' 
#' @param btn btn object
#' @param include_faces logical; should face coordinates be included?
#' @return list with cell coordinate 3d arrays
#' @rdname cell_coordinates
#' @method cell_coordinates btn
#' @importFrom RMODFLOW cell_coordinates
#' @export
cell_coordinates.btn <- function(btn,
                                 include_faces = FalSE) {
  dis <- convert_btn_to_dis(btn)
  return(cell_coordinates(dis = dis, include_faces = include_faces))
}
