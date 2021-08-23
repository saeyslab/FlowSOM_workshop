#' Get percentages per input file
#'
#' Computes percentages per file per cluster and metacluster
#'
#' @param fsom FlowSOM object as returned by the FlowSOM() function
#' @param meta_names Names to assign to the metaclusters. If NULL, numbers
#'                   are used.
#'
#' @return A list with cluster percentages and metacluster percentages
#'
#' @export
get_pctgs <- function(fsom, meta_names = NULL){
  cell_ids <- fsom$metaData
  files <- sapply(seq_len(length(cell_ids)),
                  function(i){
                    rep(gsub(".*/", "", names(cell_ids)[i]),
                        cell_ids[[i]][2] - cell_ids[[i]][1] + 1)
                  }) %>%
    unlist()
  pctgs <- table(files, GetClusters(fsom)) %>%
    as.matrix() %>%
    apply(1, function(x){x/sum(x)}) %>%
    t()
  pctgs_meta <- table(files, GetMetaclusters(fsom)) %>%
    as.matrix() %>%
    apply(1, function(x){x/sum(x)}) %>%
    t()
  if(!is.null(meta_names)) colnames(pctgs_meta) <- meta_names
  return(list("pctgs" = as.matrix(pctgs),
              "pctgs_meta" = as.matrix(pctgs_meta)))
}
