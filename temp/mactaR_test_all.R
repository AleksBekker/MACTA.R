#This is the script that must be running every day.
#My own path for my env, we can use the session info to standardize versions
library(pheatmap)
library(Seurat)
library(devtools)
.libPaths("/home/nk270/R-4.0.1/library/")
setwd("/home/nk270/MACTA.R")
load_all()
#Generate the obejcts - Now in dataset
data("pbmc_example")
query = pbmc_example$query
reference = pbmc_example$reference
markerList = pbmc_example$markerList
#Reproducibility
set.seed(42)

processed_reference = preprocess_reference(markerList)
processed_query = preprocess_query(query)
ucell.res = ucell_annotate(processed_query,processed_reference,ncores=4)
ucell.labels = ucell_convert(ucell.res)
pheatmap(as.data.frame.matrix(table(ucell.labels,query$cell_type)),scale="row",cluster_rows=F,cluster_cols=F)


#Test if it can run the classifier
scpred.path = "/n/scratch3/users/n/nk270/scPred_classifier_test.RDS"
file.remove(scpred.path)
processed_reference = scpred_preprocess_reference(reference,
                                           scPred_classifier_path = scpred.path)
#Test if it can load the classifier
processed_reference = scpred_preprocess_reference(reference,
                                           scPred_classifier_path = scpred.path)
processed_query = preprocess_query(query)
scpred.res = scpred_annotate(processed_query,processed_reference)
scpred.labels = scpred_convert(scpred.res)
pheatmap(as.data.frame.matrix(table(scpred.labels,query$cell_type)),scale="row",cluster_rows=F,cluster_cols=F)

processed_query = garnet_preprocess_query(query)
processed_reference = garnet_preprocess_reference(reference, markerList,
                                                     garnett_classifier_path="/n/scratch3/users/n/nk270/garnett_classifier_test.RDS")
garnet.res = garnett_annotate(processed_query, processed_reference)
garnet.labels = garnett_convert(garnet.res,labels="labels",score_scaling = "raw")
garnet.scores = garnett_convert(garnet.res,labels="scores",score_scaling = "raw")

pheatmap(as.data.frame.matrix(table(garnet.labels,query$cell_type)),scale="row",cluster_rows=F,cluster_cols=F)

processed_query = preprocess_query(query)
processed_reference = preprocess_reference(reference)
sccatch_list_tissues()
#annotation is cluster level!!!!
sccatch.res = sccatch_annotate(seur_obj = processed_query,tissue = "Blood",species="Human")
pheatmap(as.data.frame.matrix(table(sccatch.res@celltype$cluster, sccatch.res@celltype$cell_type)))

processed_query = preprocess_query(query)
processed_reference = preprocess_reference(reference)
singler.res = singler_annotate(query,reference)
singler.labels = singler_convert(singler.res)
pheatmap(as.data.frame.matrix(table(singler.labels,query$cell_type)),scale="row",cluster_rows=F,cluster_cols=F)

processed_query = preprocess_query(query)
processed_reference = preprocess_reference(markerList)
scina.res = scina_annotate(processed_query,processed_reference)
scina.labels = scina_convert(scina.res)
pheatmap(table(scina.labels,query$cell_type),scale="row",cluster_rows=F,cluster_cols=F)

processed_query = preprocess_query(query)
processed_reference = symphony_preprocess_reference(reference)

processed_query = preprocess_query(query)
processed_reference = scmrma_preprocess_reference(markerList)
scMRMA.res = scMRMA_annotate(query)
scMRMA.labels = scMRMA_convert(scMRMA.res)
pheatmap(table(scMRMA.labels,query$cell_type),scale="row",cluster_rows=F,cluster_cols=F)

processed_query = preprocess_query(query)
processed_reference = scmap_preprocess_reference(reference)
scmap.res = scmap_annotate(processed_query,processed_reference)
scmap.labels = scmap_convert(scmap.res)
pheatmap(table(scmap.res$combined_labs,query$cell_type),scale="row",cluster_rows=F,cluster_cols=F)
