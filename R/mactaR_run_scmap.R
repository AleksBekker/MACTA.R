library(scmap)
library(SingleCellExperiment)

#' Create an index cell from a seurat object using scmap.
#' TODO: Add one for indexCluster according to their vignette. Done
#' @param reference - Seurat object of reference
#' @param assay - Assay to use. Uses both count and data (log counts)
scmap_preprocess_reference <- function(reference, assay="RNA",mode="cluster"){
  reference$celltype = Idents(reference)
  sce <- SingleCellExperiment(assays = list(counts = GetAssayData(reference,assay=assay,slot="counts")),
                              colData = reference@meta.data)
  rowData(sce)$feature_symbol <- rownames(sce)
  logcounts(sce) <-  GetAssayData(reference,assay=assay,slot="data")
  sce <- selectFeatures(sce, suppress_plot = T) 
  if(mode=="cluster"){
    sce <- indexCluster(sce,cluster_col="celltype")  
  }else if(mode=="cell"){
    sce <- indexCell(sce)  
  }
  return(sce)
}

#' Wrapper around scmap function.
#' @param processed_query - Query seurat object after any processing you wish to apply. Uses counts and log(counts) from the data slot
#' @param processed_refernece - Reference Seurat object produced by scmap_preprocess_reference
#' @param mode - cluster or cell mode, requires the appropriate pre-processing function
#' @export
scmap_annotate <- function(processed_query, processed_reference,assay="RNA",mode="cluster"){
  
  sce <- SingleCellExperiment(assays = list(counts = GetAssayData(processed_query,assay=assay,slot="counts")),
                              colData = processed_query@meta.data)
  rowData(sce)$feature_symbol <- rownames(sce)
  logcounts(sce) <-  GetAssayData(processed_query,assay=assay,slot="data")
  sce <- selectFeatures(sce, suppress_plot = T) 
  if(mode=="cell"){
    scmap.res <- scmapCell(
      projection = sce, 
      index_list = list(
        ref = metadata(processed_reference)$scmap_cell_index
      )
    )
  }else if(mode=="cluster"){
    scmap.res <- scmapCluster(
      projection = sce, 
      index_list = list(
        ref = metadata(processed_reference)$scmap_cluster_index
      )
    )
  }
  return(scmap.res)
}

#' Convert scmap results to label vector
#' @param scmap.res - Result from scmap_annotate
#' @param labels - "labels" or "score" - Only labels is implemented
#' @param score_scaling - "raw", "center_scale", "softmax" - How to process scores
#' @export
scmap_convert <- function(scmap.res,labels="labels",score_scaling="raw"){
  if(labels=="labels"){
    return(scmap.res$combined_labs)
  }else{
    stop("Not implemented")
  }
}
