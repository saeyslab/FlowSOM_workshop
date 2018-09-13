manual_vector <- function(manual_matrix, cell_types){
  manual <- rep("Unknown",nrow(manual_matrix))
  for(cellType in cell_types){
    manual[manual_matrix[,cellType]] <- cellType
  }
  manual <- factor(manual, levels=c("Unknown",cell_types))
  return(manual)
}
