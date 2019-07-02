#'@export
plot_cluster_MFIs <- function(fsom, names = NULL){
  MFIs <- GetMFIs(fsom)[, fsom$FlowSOM$map$colsUsed]
  plot_MFIs(MFIs, fsom, names)
}

#'@export
plot_metacluster_MFIs <- function(fsom, names = NULL){
  MFIs <- MetaclusterMFIs(fsom)[, fsom$FlowSOM$map$colsUsed]
  plot_MFIs(MFIs, fsom, names)
}

plot_MFIs <- function(MFIs, fsom, names){
  colnames(MFIs) <- fsom$FlowSOM$prettyColnames[fsom$FlowSOM$map$colsUsed]
  if(!is.null(names)) rownames(MFIs) <- paste0(rownames(MFIs), " (", names, ")")
  pheatmap::pheatmap(MFIs)
}
