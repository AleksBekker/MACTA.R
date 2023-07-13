get_assay_data <- function(object, assay) {
  if (is(object, "Matrix") || is(object, "matrix") || is(object, "list")) {
    return(object)
  }

  if (is(object, "Seurat")) {
    return(Seurat::GetAssayData(object, assay))
  }

  # if (is(object, "SummarizedExperiment")) {
  #   return(SummarizedExperiment::assay(object, assay))
  # }

  if (is(object, "data.frame")) {
    return(as.matrix(object))
  }

  stop("Invalid `object` in `get_assay_data`")
}
