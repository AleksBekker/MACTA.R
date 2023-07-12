library(scCATCH)
library(Seurat)

# Simply lists the scCATCH available tissues
# @param demo_geneinfo - marker list from scCATCH, you can insert your own
sccatch_list_tissues <- function(demo_marker = cellmatch){
  return(unique(demo_marker$tissue))
}
sccatch_list_species <- function(demo_marker = cellmatch){
  return(unique(demo_marker$species))
}
# Wrapper around the scCATCH function. Needs a seurat object, assay and slot, as well as pre-made clustering (you can pre-process this with Seurat)
# Finally, needs tissue
# @param seur_obj - Your seurat object
# @param assay - Which assay you'll use
# @param slot - Which slot of that assay you'll use, it expects normalized data
# @param cluster_column - Clustering column, make sure it's R-compliant to be sure
# @param geneinfo - Gene info dataframe, check vignette here for format: https://cran.r-project.org/web/packages/scCATCH/vignettes/tutorial.html
# @param Tissue - Uhh, Tissue.
# @param Species - Uhh, Species.
preprocess_query <- function(seur_obj){
  return(seur_obj)
}

preprocess_reference <- function(...){
  return("Preprocess reference not needed for scCATCH")
}

sccatch_annotate <- function(seur_obj, geneinfo,cluster_column, tissue,species,...){
  args = list(
    ...,
    assay="RNA",
    slot="data"
  )
  mat <- rev_gene(data = GetAssayData(seur_obj,assay = args$assay,slot = args$slot), data_type = "data", species = species, geneinfo = geneinfo)
  obj <- createscCATCH(data = mat, cluster = unlist(seur_obj[[cluster_column]]))
  obj <- findmarkergene(object = obj, species = "Human", marker = cellmatch, tissue = tissue,use_method = "1")
  obj <- findcelltype(object = obj)
  labels <- obj@celltype
  return(obj)
}
