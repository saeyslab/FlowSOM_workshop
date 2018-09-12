#' Parse a FlowJo workspace for one specific file
#'
#' Extract the compensated and transformed data and all gate labels.
#'
#' @param file     Path to the fcs file of interest
#' @param wsp_file Path to the FlowJo workspace
#' @param group    Group the file belongs to in the FlowJo workspace.
#'                 Default is "All Samples"
#'
#' @return Returns a list with two elements: flowFrame and gates.
#' \code{flowFrame} contains the compensated and transformed fcs file, as set
#' up in the FlowJo workspace.
#' \code{gates} is a matrix in which each row corresponds with an individual
#' cell and each column with an individual gate, containing TRUE/FALSE values
#' indicating whether the cell falls into that gate
#'
#' @export
parse_flowjo <- function(files,
                         wsp_file,
                         group = "All Samples",
                         plot = FALSE) {
  wsp <- flowWorkspace::openWorkspace(wsp_file)
  o <- capture.output(
    gates <- suppressMessages(flowWorkspace::parseWorkspace(wsp, group))
  )
  files_in_wsp <- gates@data@origSampleVector
  counts <- as.numeric(gsub(".*_([0-9]*)$", "\\1", files_in_wsp))
  result <- list()
  for(file in files){
    print(paste0("Processing ", file))
    file_id <- grep(gsub(".*/", "", file), files_in_wsp)
    gate_names <- flowWorkspace::getNodes(gates[[file_id]], path = "auto")
    gatingMatrix <- matrix(FALSE,
                           nrow = counts[file_id],
                           ncol = length(gate_names),
                           dimnames = list(NULL, gate_names))
    for (gate in gate_names) {
      gatingMatrix[, gate] <- flowWorkspace::getIndiceMat(gates[[file_id]],
                                                          gate)
    }
    ff <- flowWorkspace::getData(gates[[file_id]], "root")
    result[[file]] <- list("flowFrame" = ff,
                           "gates" = gatingMatrix)

    if (plot) {
      flowWorkspace::plot(gates[[file_id]])
    }
  }
  if (length(files == 1)) result <- result[[1]]
  return(result)
}

