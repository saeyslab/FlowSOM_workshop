FlowSOM_subset <- function(fsom, file){
  fsom_selected <- FlowSOMSubset(fsom$FlowSOM,
                                 fsom$FlowSOM$metaData[[file]][1]:fsom$FlowSOM$metaData[[file]][2])
  return(list(FlowSOM = fsom_selected, metaclustering = fsom$metaclustering))
}
