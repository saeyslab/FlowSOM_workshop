#' Get counts per input file
#'
#' Computes counts per file per cluster and metacluster
#'
#' @param fsom FlowSOM object as returned by the FlowSOM() function
#' @return A list with cluster counts and metacluster counts
#'
#' @export
get_counts <- function(fsom){
  cell_ids <- fsom$FlowSOM$metaData
  files <- sapply(seq_len(length(cell_ids)),
                  function(i){
                    rep(gsub(".*/", "", names(cell_ids)[i]),
                        cell_ids[[i]][2] - cell_ids[[i]][1] + 1)
                  }) %>%
    unlist()
  counts <- table(files, GetClusters(fsom$FlowSOM))
  counts_meta <- table(files, GetMetaclusters(fsom))
  return(list("counts" = as.matrix(counts),
              "meta_counts" = as.matrix(counts_meta)))
}
