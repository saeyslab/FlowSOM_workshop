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
plot_aggregate <- function(input,
                           labels = NULL,
                           channels = NULL,
                           sample_names = NULL,
                           output_image = "aggregate.png",
                           max_nm = 150000){
  if(is(input, "flowSet")){
    data <- flowCore::fsApply(input, function(ff){ ff@exprs })
    cell_counts <- flowCore::fsApply(input, function(ff){ nrow(ff) })
    file_values <- unlist(sapply(seq_len(length(cell_counts)),
                                 function(i){
                                   rep(i, cell_counts[i])
                                 }))
    ff <- input[[1]]
  } else if(is(input, "flowFrame")){
    data <- input@exprs
    cell_counts <- table(data[,"File"])
    file_values <- data[,"File"]
    ff <- input
  }

  if(is.null(sample_names)) sample_names <- names(cell_counts)

  subset <- sample(seq_len(nrow(data)), min(max_nm, nrow(data)))
  data <- data[subset, ]
  file_values <- file_values[subset]
  file_values_scattered <- file_values + stats::rnorm(length(file_values),
                                                      0, 0.1)

  if(is.null(channels)) channels <- colnames(data)

  # Try to make the plot as square as possible
  nrows_in_plot <- floor(sqrt(length(channels)))
  ncols_in_plot <- ceiling(length(channels) / nrows_in_plot)

  # Write to a png for minimal overhead of plotting so many points
  png(output_image,
      width = 6000, height = 3000)
  # Increase margins of the plots to have enough space for annotation
  par(mar = c(25.1, 12.1, 2.1, 2.1))
  # Plot multiple plots in one figure
  layout(matrix(seq_len(nrows_in_plot * ncols_in_plot),
                nrow = nrows_in_plot,
                byrow = TRUE))

  if(is.null(labels)){
    colors <- "#00000055"
  } else {
    labels <- factor(labels)
    color_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
    colors <- paste0(color_palette(length(levels(labels))), "55")
    colors <- colors[labels][file_values]
  }
  # Make a plot for every channel
  for(channel in channels){
    print(FlowSOM::get_markers(ff, channel))
    # Empty plot, with ranges set correctly
    plot(0, type="n", xaxt="n",
         xlab = "",
         ylab = FlowSOM::get_markers(ff, channel),
         cex.lab = 5,
         ylim = c(min(data[, channel]),
                  max(data[, channel])),
         xlim = c(0, length(cell_counts)+1))
    # Add light grey vertical lines
    abline(v = seq_len(length(cell_counts)), col="lightgrey")
    # Add file names on x-axis
    axis(side =  1,
         at = seq_len(length(cell_counts)),
         labels = sample_names,
         las = 2,
         cex.axis = 3)
    # Add the actual data points
    points(file_values_scattered, data[,channel],
           pch = ".",
           col = colors)
  }
  dev.off()
}
