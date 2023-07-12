library(BiocParallel)
library(SingleR)

# Wrapper around the SingleR function
# @param processed_query - Seurat object for query
# @param processed_reference - Seurat object for reference
# @param ncores - Number of cores to use, registers them with BiocParallel
# @param assay - Seurat Assay to use
# @param slot - Seurat slot to use
singler_annotate <- function(processed_query, processed_reference,ncores=1,assay="RNA",slot="scale.data"){
  if(ncores > 1){
    coreParam <- MulticoreParam(workers = ncores)
  }else{
    coreParam <- SerialParam()
  }
  singler.res = SingleR(
    test = GetAssayData(processed_query,
                        assay=assay,
                        slot=slot),
    ref = GetAssayData(processed_reference,
                       assay=assay,
                       slot=slot),
    labels=Idents(processed_reference),
    BPPARAM = coreParam
  )
  return(singler.res)
}

singler_convert <- function(singler.res,labels="labels",score_scaling="raw"){
  labels.df = singler.res$scores
  return(convert_generic(labels.df,score_scaling=score_scaling,labels=labels))
}
