#'@export
plot_pctgs <- function(pctgs){
  data_to_plot <- pctgs %>%
    as.data.frame() %>%
    tibble::rownames_to_column("File") %>%
    tidyr::gather("Cluster", "Freqs", -"File")
  ggplot2::ggplot(data_to_plot) +
    ggplot2::geom_point(ggplot2::aes(x = File, y = Freqs)) +
    ggplot2::facet_wrap(~ Cluster, ncol = 2, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::ylim(0, NA)
}
