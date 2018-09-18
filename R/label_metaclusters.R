number_duplicates <- function(x){
  counts <- table(x)
  for (value in names(counts)) {
    if (counts[value] > 1) {
      x[which(x == value)] <- paste0(value, "_", seq_len(counts[value]))
    }
  }
  return(x)
}

#' @export
label_metaclusters <- function(fsom, manual_labels){
  counts <- as.matrix(table(FlowSOM::GetMetaclusters(fsom),
                            manual_labels))
  metacluster_names <- apply(counts,
                             1,
                             function(x) colnames(counts)[which.max(x)])
  metacluster_names <- number_duplicates(metacluster_names)
  return(metacluster_names)
}
