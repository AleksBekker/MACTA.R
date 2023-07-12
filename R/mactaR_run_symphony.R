library(symphony)
library(Seurat)

symphony_preprocess_reference <- function(reference_object,
                                          assay="RNA",
                                          slot="data",
                                          batch_var=NULL,
                                          symphony_path=NULL){
  reference_object$celltype = Idents(reference_object)
  reference = buildReference(
    GetAssayData(reference_object,assay=assay,slot=slot),
    reference_object@meta.data,
    vars = batch_var,         # variables to integrate over
    K = 100,                   # number of Harmony clusters
    verbose = TRUE,            # verbose output
    do_umap = TRUE,            # can set to FALSE if want to run umap separately later
    do_normalize = FALSE,      # set to TRUE if input counts are not normalized yet
    vargenes_method = 'vst',   # method for variable gene selection ('vst' or 'mvp')
    vargenes_groups = batch_var, # metadata column specifying groups for variable gene selection 
    topn = 2000,               # number of variable genes to choose per group
    d = 20,                    # number of PCs
    save_uwot_path = symphony_path
  )
  return(reference)
}

symphony_anotate <- function(query,reference,assay="RNA",slot="data"){
  query_exp = GetAssayData(query,assay=assay,slot=slot)
  query = mapQuery(query_exp,             # query gene expression (genes x cells)
                   query@meta.data,        # query metadata (cells x attributes)
                   reference,             # Symphony reference object
                   do_normalize = FALSE,  # perform log(CP10k+1) normalization on query
                   do_umap = TRUE)        # project query cells into reference UMAP
  symphony.res <- knnPredict(
    query,
    reference,
    reference$celltype,
    k = 5,
    save_as = "cell_type_pred_knn",
    confidence = TRUE,
    seed = 0
  )
  return(symphony.res)
}

symphony_convert <- function(symphony.res,labels="labels",score_scaling="raw"){
  if(labels=="labels"){
    return(symphony.res$cell_type_pred_knn)
  }else{
    stop("Not implemented for Symphony")
  }
}
