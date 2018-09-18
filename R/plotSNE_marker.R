#' plotSNE_marker
#'
#' Function to plot the result of a tSNE, colored according to the expression
#' values of a certain marker of interest.
#'
#' @param flowframe The flowframe containing the data on which the tSNE was
#' computed
#' @param tsne_result The resulting object of the Rtsne::Rtsne function
#' @param marker The marker of interest, given as a character
#'
#' @return ggplot of the tSNE result colored according to the expression of the
#' provided marker
#' @export
#'
#' @examples
#'
#'
plotSNE_marker <- function(flowframe, tsne_result, marker){
  df <- as.data.frame(cbind(tsne_result$Y, flowframe@exprs))
  colnames(df) <- c("V1","V2",
                    as.character(FlowSOM::get_markers(flowframe,
                                                      flowCore::colnames(flowframe))))
  colnames(df) <- gsub("-","_",colnames(df))
  p<- ggplot2::ggplot(df, ggplot2::aes(V1, V2, col = !!rlang::sym(marker))) +
    ggplot2::geom_point()+
    ggplot2::theme_minimal()
  return(p)
}


