install.packages("devtools")
install.packages("Rtsne")
install.packages("tidyverse")

# Bioconductor packages , to be installed through biocLite:

source("https://bioconductor.org/biocLite.R")
biocLite("flowCore")
biocLite("flowWorkspace")
biocLite("flowAI")
biocLite("flowDensity")

# FlowSOM and flowSOM_workshop packages, to be installed directly from the
# github repository:

devtools::install_github("saeyslab/FlowSOM")
devtools::install_github("saeyslab/FlowSOM_workshop", build_vignettes = TRUE)
