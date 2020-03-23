simulate_batch <- function(preprocessed_dir,
                           files,
                           channels_of_interest){
  # Create matrix with transformation values
  n <- length(channels_of_interest)
  transformation <- matrix(NA,
                           nrow = n,
                           ncol = 2,
                           dimnames = list(channels_of_interest, # Other transformation values for each channel
                                           c("shift", "scale"))) # 2 transformations
  set.seed(170320)
  transformation[,"shift"] <- sample(c(700:800, 1200:1300), n)/1000
  transformation[,"scale"] <- sample(700:1300, n)/1000
  
  # Loop over preprocessed files and apply transformations
  # files <- list.files(preprocessed_dir, pattern = "Live.fcs")
  for (file in file.path(preprocessed_dir, files)){
    ff <- read.FCS(file)
    for (channel in channels_of_interest){
      if (channel == "PE-A"){
        gate <- deGate(ff, channel)
        neg <- ff@exprs[,channel] < gate
        ff@exprs[,channel][neg] <- scales::rescale(ff@exprs[,channel][neg],
                                                   to=c(min(ff@exprs[,channel][neg]),
                                                        max(ff@exprs[,channel][neg]*1.1)))
        ff@exprs[,channel][!neg] <- scales::rescale(ff@exprs[,channel][!neg],
                                                    to=c(min(ff@exprs[,channel][!neg]),
                                                         max(ff@exprs[,channel][!neg]*0.8)))*0.8
      }
      ff@exprs[,channel] <- ff@exprs[,channel] * transformation[channel, "shift"]
      ff@exprs[,channel] <- scales::rescale(ff@exprs[,channel],
                                            to=c(min(ff@exprs[,channel]),
                                                 max(ff@exprs[,channel])*transformation[channel, "scale"]))
      
    }
    write.FCS(ff, sub("Live", "Batch", file))
  }
  
  # Aggregate of both original and transformed files
  files_batch <- list.files(preprocessed_dir, pattern = "Batch.fcs")
  ff_agg <- AggregateFlowFrames(file.path(preprocessed_dir, c(files, files_batch)),
                                cTotal = 3000000,
                                writeMeta = TRUE,
                                outputFile = file.path(agg_dir, "aggregate_batch.fcs"))
  
  plot_aggregate(input = ff_agg, 
                 labels = rep(c("1", "2"), each = length(files)),
                 sample_names = c(gsub(".*_(Tube_[0-9]*)_.*", "\\1", files), 
                                  gsub(".*_(Tube_[0-9]*)_.*", "\\1", files_batch)),
                 channels = channels_of_interest,
                 output_image = file.path(preprocessed_dir, "aggregate_batch.png"))
  
}