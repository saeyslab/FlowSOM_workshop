# files <- list.files("inst/extdata/", pattern = ".fcs$",
#                     full.names = TRUE)
# for (file in files) {
#   ff <- FlowCore::read.FCS(file)
#   ff@parameters@data[,"desc"] <- gsub("#.*", "", ff@parameters@data[,"desc"] )
#   FlowCore::write.FCS(ff,
#             file)
# }
