#' @export
compute_wilcox <- function(values, group1, group2){
  p_values <- rep(NA, ncol(values))
  for(i in seq_len(ncol(values))){
    test <- wilcox.test(values[group1, i],
                        values[group2, i])
    p_values[i] <- test$p.value
  }
  adj_p <- stats::p.adjust(p_values, "BH")

  diff <- sign(apply(values[group1, ], 2, median) - apply(values[group2, ], 2, median))
  res <- cbind("P value" = p_values,
               "Adjusted p value" = adj_p,
               "- Log10 p" = -log10(adj_p),
               "Direction" = diff,
               "Fold change" = pmax(apply(values[group1, ], 2, median),
                                    apply(values[group2, ], 2, median)) /
                 pmin(apply(values[group1, ], 2, median),
                      apply(values[group2, ], 2, median)))

  res <- res[order(res[,1]), ]
  return(res)
}
