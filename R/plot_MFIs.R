#'@export
plot_cluster_MFIs <- function(fsom){
  MFIs <- GetMFIs(fsom)[, fsom$FlowSOM$map$colsUsed]
  colnames(MFIs) <- fsom$FlowSOM$prettyColnames[fsom$FlowSOM$map$colsUsed]
  pheatmap::pheatmap(MFIs)
}

#'@export
plot_metacluster_MFIs <- function(fsom, names = NULL){
  MFIs <- MetaclusterMFIs(fsom)[, fsom$FlowSOM$map$colsUsed]
  colnames(MFIs) <- fsom$FlowSOM$prettyColnames[fsom$FlowSOM$map$colsUsed]
  if(!is.null(names)) rownames(MFIs) <- names
  pheatmap::pheatmap(MFIs)
}
