files <- list.files("inst/extdata/", pattern = ".fcs$",
                    full.names = TRUE)
for (file in files) {
  ff <- read.FCS(file)
  ff@parameters@data[,"desc"] <- gsub("#.*", "", ff@parameters@data[,"desc"] )
  write.FCS(ff,
            file)
}
