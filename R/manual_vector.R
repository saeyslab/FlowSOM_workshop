#' Summarise the gating matrix into one vector, only including the cell types of
#' interest
#'
#' Extract the compensated and transformed data and all gate labels.
#'
#' @param manual_matrix Matrix containing boolean values, indicating for every
#'                      gate (column) whether the cell (row) is part of it or not.
#' @param cell_types Cell types to use in the summary vector. All others will be
#'                   ignored and cells which do not fall in one of these gates
#'                   will get the label "Unknown". Order is important!
#'
#' @return A factor with one label for every cell
#'
#' @export
manual_vector <- function(manual_matrix, cell_types){
  manual <- rep("Unknown",nrow(manual_matrix))
  for(cellType in cell_types){
    manual[manual_matrix[,cellType]] <- cellType
  }
  manual <- factor(manual, levels=c("Unknown",cell_types))
  return(manual)
}
