
source("mactaR_run_UCell.R")
processed_reference = preprocess_reference(markerList)
processed_query = preprocess_query(query)
ucell.res = ucell_annotate(processed_query,processed_reference,ncores=4)
ucell.labels = ucell_convert(ucell.res)
pheatmap(as.data.frame.matrix(table(ucell.labels,query$cell_type)),scale="row",cluster_rows=F,cluster_cols=F)
source("~/mactaR_run_scPred.R")
#Test if it can run the classifier
scpred.path = "/n/scratch3/users/n/nk270/scPred_classifier_test.RDS"
file.remove(scpred.path)
processed_reference = preprocess_reference(reference
                                           scPred_classifier_path = scpred.path)
#Test if it can load the classifier
processed_reference = preprocess_reference(reference
                                           scPred_classifier_path = scpred.path)
processed_query = preprocess_query(query)
scpred_annotated = scpred_annotate(processed_query,processed_reference)
pheatmap(as.data.frame.matrix(table(scpred_annotated,query$cell_type)),scale="row",cluster_rows=F,cluster_cols=F)

source("~/mactaR_run_garnett.R")
processed_query = seurat_to_garnettcds.query(query)
processed_reference = seurat_to_garnettcds.reference(reference, markerList,
                                                     garnett_classifier_path="/n/scratch3/users/n/nk270/garnett_classifier_test.RDS")
garnett_annotated = garnett_annotate(processed_query, processed_reference)
pheatmap(as.data.frame.matrix(table(garnett_annotated,query$cell_type)),scale="row",cluster_rows=F,cluster_cols=F)

source("~/mactaR_run_scCATCH.R")
processed_query = preprocess_query(query)
processed_reference = preprocess_reference(reference)
sccatch_list_tissues()
sccatch_annotated = sccatch_annotate(seur_obj = processed_query,geneinfo = scCATCH::geneinfo,cluster_column = "cell_type",tissue = "Blood",species="Human")

source("~/mactaR_run_SingleR.R")
processed_query = preprocess_query(query)
processed_reference = preprocess_reference(reference)
singler.res = singler_annotate(query,reference)
singler.labels = singler_convert(singler.res)
pheatmap(as.data.frame.matrix(table(singler.labels,query$cell_type)),scale="row",cluster_rows=F,cluster_cols=F)

