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
parse_flowjo <- function(file,
                         wsp_file,
                         group = "All Samples") {
  wsp <- flowWorkspace::openWorkspace(wsp_file)
  o <- capture.output(
    gates <- suppressMessages(flowWorkspace::parseWorkspace(wsp, group))
  )
  files <- gates@data@origSampleVector
  counts <- as.numeric(gsub(".*_([0-9]*)$", "\\1", files))
  file_id <- grep(gsub(".*/", "", file), files)
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
  return(list("flowFrame" = ff,
              "gates" = gatingMatrix))
}

