library(Seurat)
library(scPred)

scpred_preprocess_reference = function(seur_obj,celltype_column,assay="RNA",slot="data",scPred_classifier_path=NULL){
  mat = GetAssayData(seur_obj,assay=assay,slot=slot)
  seur_obj$celltype = Idents(seur_obj)
  if(!is.null(scPred_classifier_path)){
    if(file.exists(scPred_classifier_path)){
      classifier = readRDS(scPred_classifier_path)
    }else{
      classifier <- getFeatureSpace(seur_obj, "celltype") #Get informatice PCs
      classifier <- trainModel(classifier)
      saveRDS(classifier,scPred_classifier_path)
    }
    }else{
      classifier <- getFeatureSpace(seur_obj, "celltype") #Get informatice PCs
      classifier <- trainModel(classifier)
    }
  return(classifier)
}

scpred_annotate <- function(query_obj, reference_obj){
  query_obj = scPredict(query_obj, reference_obj)
  return(query_obj)
}
scpred_convert <- function(scpred.res,labels="labels",score_scaling="raw"){
  labels.df = scpred.res@meta.data[,grepl("scpred_",colnames(scpred.res@meta.data))]
  labels.df = labels.df[,setdiff(colnames(labels.df),c("scpred_max","scpred_prediction","scpred_no_rejection"))]
  colnames(labels.df) = gsub("scpred_","",colnames(labels.df))
  return(convert_generic(labels.df,labels=labels,score_scaling = score_scaling))
}
