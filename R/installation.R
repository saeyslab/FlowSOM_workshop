installation_flowSOMworkshop <- function(){
  install.packages("devtools")
  install.packages("Rtsne")
  install.packages("tidyverse")
  install.packages("openxlsx")
  install.packages("mvtnorm")

  # Bioconductor packages , to be installed through BiocManager:

  install.packages("BiocManager")
  BiocManager::install("flowCore")
  BiocManager::install("flowWorkspace")
  BiocManager::install("flowAI")
  BiocManager::install("flowDensity")
  BiocManager::install("CytoML")


  # FlowSOM and flowSOM_workshop packages, to be installed directly from the
  # github repository:

  devtools::install_github("saeyslab/FlowSOM")
  devtools::install_github("saeyslab/FlowSOM_workshop")
}


