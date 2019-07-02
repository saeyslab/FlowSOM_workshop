#' parse_markertable
#'
#' Function to parse a marker table with celltypes in rows and markers in
#' columns, and every value being either NA, "low" or "high". The result is a
#' list with one item for every cell type, which is a named vector with only
#' those markers which are "high"/"low".
#'
#' @param marker_table The data frame to parse
#'
#' @return a list with one item for every cell type, which is a named vector
#' with only those markers which are "high"/"low"
#' @export
#'
#' @examples
#'
#'
parse_markertable <- function(marker_table){
  dam2 = reshape2::dcast(
    dplyr::mutate(
      reshape2::melt(marker_table,id.var="Markers")),Markers~variable)

  cellTypes <- lapply(seq_along(dam2[,2]), function(i){
    dam2[i,which(!is.na(dam2[i,]))[-1]]
  })
  names(cellTypes) <- dam2[,1]
  return(cellTypes)
}
