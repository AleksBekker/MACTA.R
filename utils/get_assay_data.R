
library(Seurat)
library(SummarizedExperiment)

get_assay_data <- function(object, assay) {
  if (is(object, "Matrix") | is(object, "matrix")) {
    return(object)
  }
  
  if (is(object, "Seurat")) {
    return(GetAssayData(object, assay))
  }

  if (is(object, "SummarizedExperiment")) {
    return(assay(object, assay))
  }
  
  if (is(object, "data.frame")) {
    return(as.matrix(object))
  }
}
