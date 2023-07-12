library(Seurat)
library(pheatmap)
#Load two random objects
reference <- scPred::pbmc_1
query <- scPred::pbmc_2
query <- NormalizeData(query)
query <- FindVariableFeatures(query)
query <- ScaleData(query)
query <- RunPCA(query)
reference <- NormalizeData(reference)
reference <- FindVariableFeatures(reference)
reference <- ScaleData(reference)
reference <- RunPCA(reference)
Idents(reference) = reference$cell_type
Idents(query) = query$cell_type
#Get markers and convert to the list format I use
reference.markers = FindAllMarkers(reference,only.pos=T,logfc.threshold = 0.5)
reference.markers.split = split(reference.markers, reference.markers$cluster)
markerList = lapply(reference.markers.split, function(x) x$gene[1:10])
markerList = lapply(markerList, function(x){ x[!is.na(x)]})