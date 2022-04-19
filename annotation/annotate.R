
library(Seurat)
library(SingleCellExperiment)

source("utils/get_assay_data.R")

# Import custom annotation tool interfaces -------------------------------------

source("annotation/tool_interfaces/SingleR_interface.R")

# `annotate` function implementation -------------------------------------------

annotate <- function(expr_data, ref_data, labels,
                     annot_type, annot_tools = "*", convert_to = "labels",
                     tool_interfaces = list(
                       ref = list(
                         "SingleR" = SingleR_interface
                       ),
                       marker = list()
                     ), ...) {
  #' Performs multi-tool auto-CTA
  #'
  #' @param expr_data `matrix` or `Matrix` of experimental data, 
  #' on which CTA is performed
  #' @param ref_data `matrix` or `Matrix` of reference/marker data 
  #' for the CTA analysis
  #' @param labels `character` vector that holds the labels for each row of 
  #' `ref_data`
  #' @param annot_type which type of CTA to perform <marker/ref>
  #' @param annot_tools `character` vector of the names of the tools to be used.
  #' '*' selects all of the tools in the `annot_type` category
  #' @param convert_to `character` describing the format of the output data
  #' @param tool_interfaces list of interfaces the program will use
  #'
  #' @returns a list of  test_name -> test_results
  
  # Merge default arguments with ... arguments
  args = list(
    expr_type = "logcounts",
    ref_type = "logcounts"
  ) %>% c(list(...), .)

  # Get `expr_data` and `ref_data` assays from within original objects
  expr_data <- get_assay_data(expr_data, args$expr_type)
  ref_data <- get_assay_data(ref_data, args$ref_type)
  
  # Process `annot_tools`
  if (identical(annot_tools, "*")) {
    annot_tools <- names(tool_interfaces[[annot_type]])
  }
  
  lapply(
    tool_interfaces[[annot_type]][annot_tools],
    function(x) run_full(x, expr_data, ref_data, labels, convert_to, 
                         expr_type = expr_type, ref_type = ref_type, ...)
  ) %>% return()
}
