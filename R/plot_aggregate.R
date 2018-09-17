#' Plot Aggregate
#'
#' Plot aggregate figure
#'
#' @param flowset FlowSet to plot
#' @param labels  If not NULL, colors the files per label.
#' @param output_image Path to write the png figure to.
#'
#' @return nothing but a plot is generated
#'
#' @export
plot_aggregate <- function(flowset,
                           labels = NULL,
                           output_image = "aggregate.png"){
  data <- flowCore::fsApply(flowset, function(ff){ ff@exprs })
  cell_counts <- flowCore::fsApply(flowset, function(ff){ nrow(ff) })
  file_values <- unlist(sapply(seq_len(length(cell_counts)),
                                  function(i){
                                    rep(i, cell_counts[i])
                                  }))
  file_values_scattered <- file_values + stats::rnorm(length(file_values),
                                                      0, 0.1)

  channels <- colnames(data)

  # Try to make the plot as square as possible
  nrows_in_plot <- floor(sqrt(length(channels)))
  ncols_in_plot <- ceiling(length(channels) / nrows_in_plot)

  # Write to a png for minimal overhead of plotting so many points
  png(output_image,
      width = 6000, height = 3000)
  # Increase margins of the plots to have enough space for annotation
  par(mar = c(12.1, 12.1, 2.1, 2.1))
  # Plot multiple plots in one figure
  layout(matrix(seq_len(nrows_in_plot * ncols_in_plot),
                nrow = nrows_in_plot,
                byrow = TRUE))

  if(is.null(labels)){
    colors <- "#00000055"
  } else {
    labels <- factor(labels)
    colors <- colorRampPalette(RColorBrewer::brewer.pal("Paired", 9))(length(levels(labels)),
                      alpha = 0.4)[labels][file_values]
  }
  # Make a plot for every channel
  for(channel in sort(channels)){
    print(channel)
    # Empty plot, with ranges set correctly
    plot(0, type="n", xaxt="n",
         xlab = "",
         ylab = get_markers(flowset[[1]], channel),
         cex.lab = 5,
         ylim = c(min(data[, channel]),
                  max(data[, channel])),
         xlim = c(0, length(flowset)+1))
    # Add light grey vertical lines
    abline(v = seq_len(length(flowset)), col="lightgrey")
    # Add single stain names on x-axis
    axis(side =  1,
         at = seq_len(length(flowset)),
         labels = rownames(cell_counts),
         las = 2,
         cex.axis = 3)
    # Add the actual data points
    points(file_values_scattered, data[,channel],
           pch = ".",
           col = colors)
  }
  dev.off()
}
