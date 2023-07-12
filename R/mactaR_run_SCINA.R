#Amazingly simple to run
library(SCINA)
library(Seurat)
#Wrapper around SCINA
# @param processed_query - Seurat object for query
scina_annotate <- function(processed_query,
                           markerlist,
                           assay="RNA",
                           slot="scale.data",
                           rm_overlap = FALSE,
                           allow_unknown = TRUE){
  mat = GetAssayData(processed_query, assay=assay,slot=slot)
  markerlist = lapply(markerlist, function(x) intersect(x,rownames(mat)))
  scina.res = SCINA(exp = mat,
        signatures = markerlist,
        rm_overlap = rm_overlap,
        allow_unknown = allow_unknown
        )
  return(scina.res)
}

scina_convert <- function(scina.res,score_scaling="raw",labels="labels"){
  #Pre-process
  labels.df = t(scina.res$probabilities)
  if(labels=="labels"){
    return(scina.res$cell_labels)
  }else{
    return(convert_generic(labels.df, score_scaling,labels=labels)) 
  }
}
