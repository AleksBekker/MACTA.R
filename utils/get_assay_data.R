
library(Seurat)
library(SummarizedExperiment)

get_assay_data <- function(object, assay) {
  if (is(object, "Seurat")) {
    return(GetAssayData(object, assay))
  }

  if (is(object, "SummarizedExperiment")) {
    return(assay(object, assay))
  }
}
