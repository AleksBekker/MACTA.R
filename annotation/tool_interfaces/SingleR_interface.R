
library(magrittr)

source("annotation/tool_interfaces/cta_interface.R")



SingleR_interface <- CTAInterface(
  annotate_func = function(expr_data, ref_data, labels, expr_type = 'logcounts',
                           ref_type = 'logcounts', ...) {
    #' Performs `SingleR` annotation
    #'
    #' @param expr_data counts `dgCMatrix` of experimental data, on which CTA is
    #' performed
    #' @param ref_data reference data in the form of a logcounts `matrix` or
    #' a `SummarizedExperiment` containing such a matrix in its `logcounts`
    #' assay
    #' @param labels `character` containing labels column for `ref_data`
    #' @param expr_type `character` that describes the type of counts data in
    #' `expr_data`
    #' @param ref_type `character` that describes the type of counts data in 
    #' `ref_data`
    #'
    #' @returns `DFrame` object containing `SingleR` results
    
    # Pre-process `ref_data`

    SingleR::SingleR(
      test = expr_data,
      ref = ref_data,
      labels = labels,
      assay.type.test = expr_type,
      assay.type.ref = ref_type
    ) %>% return()
  },
  convert_func = function(results, convert_to, ...) {
    #' Converts `SingleR` results to standard format
    #'
    #' @param results `DFrame` of `SingleR` annotation results
    #' @param convert_to `character` that represents output data format.
    #' Options: <labels>
    #'
    #' @returns `SingleR` results in `convert_to` format

    switch(convert_to,
      "labels" = results$labels
    ) %>% return()
  },
  preprocess_expr_func = function(expr_data, ...) {
    
    if (is(expr_data, "Seurat")) {
      expr_data@assays$originalexp@counts %>% return()
    } 
    
    return(expr_data)
  },
  preprocess_ref_func = function(ref_data, ...) {
    
    if (is(ref_data, "Seurat")) {
      stderr("Not yet implemented")
    }
    
    return(ref_data)
  }
)
