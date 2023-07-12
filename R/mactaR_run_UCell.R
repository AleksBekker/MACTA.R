#Amazingly simple to run
library(UCell)
library(Seurat)

#Wrapper around UCell
# @param processed_query - Seurat object for query
# @param processed_reference - Seurat object for reference
# @param ncores - Number of cores to use
ucell_annotate <- function(processed_query, processed_reference,ncores=6){
  processed_query <- AddModuleScore_UCell(processed_query, features = processed_reference, ncores = ncores) #Is done with the default assay. Important to know that.
  return(processed_query)
}

ucell_convert <- function(ucell.res,score_scaling="raw",labels="labels"){
  #Pre-process
  labels.df = ucell.res@meta.data[,grepl("UCell",colnames(ucell.res@meta.data))]
  colnames(labels.df) = gsub("_UCell","",colnames(labels.df))
  
  return(convert_generic(labels.df, score_scaling,labels=labels))
}
