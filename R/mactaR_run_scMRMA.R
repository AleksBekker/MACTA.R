library(Seurat)
library(scMRMA)

#' Wrapper around the scMRMA function
#' TODO: Add the ability to insert a selfDB
#' @param species - Species
#' @param assay - Seurat object assay to use
#' @param slot - Seurat assay slot to use
#' @param selfDB - Path to selfDB file to use. Create this from marker list in subclustering.
#' @param db - Built-in db to use, only uses panglao currently
#' @export
scMRMA_annotate <- function(query,
                            species="Hsa",
                            assay="RNA",
                            slot="counts",
                            selfDB=NULL,
                            db="panglaodb"){
  if(is.null(selfDB)){
    
    scMRMA.res <- scMRMA(input = GetAssayData(query,assay=assay,slot=slot),
                         species = species,
                         db = db,
                         p = 0.05,
                         normalizedData = F,
                         selfDB = selfDB,
                         selfClusters = NULL,
                         k=20) 
  }else{
    scMRMA.res <- scMRMA(input = GetAssayData(query,assay=assay,slot=slot),
                         species = species,
                         db = NULL,
                         p = 0.05,
                         normalizedData = F,
                         selfDB = selfDB,
                         selfClusters = NULL,
                         k=20) 
  }
  return(scMRMA.res)
}

#' Function to convert the results from scMRMA to labels. No confidence is available.
#' @param scMRMA.res - Result from scMRMA_annotate
#' @param labels - Output character vector of labels or confidence scores as well? ("score")
#' @param score_scaling - Raw scores, scaled and center (center_scale), or softmaxed?
#' @param level - Annotation level to use, for PanglaoDB use 3, for your subclustering, use anything you deem appropriate.
#' @export
scMRMA_convert <- function(scMRMA.res,labels="labels",score_scaling="raw",level=3){
  if(labels=="labels"){
    return(unlist(scMRMA.res$multiR$annotationResult[[paste("Level",level,sep="")]]))
  }else if(labels=="scores"){
    stop("Not implemented error")
  }
}
