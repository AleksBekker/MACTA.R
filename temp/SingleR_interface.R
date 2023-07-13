SingleR_interface <- CTAInterface(
  rq_list = RequirementList(
    annot_type = requirement("ref", strict_validator)
  ),
  annotate = function(expr_data, ref_data, ...) {
    #' Performs `SingleR` annotation
    #'
    #' @param expr_data counts `dgCMatrix` of experimental data, on which CTA is
    #' performed
    #' @param ref_data reference data in the form of a logcounts `matrix` or
    #' a `SummarizedExperiment` containing such a matrix in its `logcounts`
    #' assay
    #'
    #' @returns `DFrame` object containing `SingleR` results

    # Get ... arguments
    args <- list(
      list(...),
      expr_type = "logcounts",
      ref_type = "logcounts"
    )

    # Run `SingleR` analysis
    SingleR::SingleR(
      test = expr_data,
      ref = ref_data,
      labels = args$labels,
      assay.type.test = args$expr_type,
      assay.type.ref = args$ref_type
    )
  },
  convert = function(results, convert_to, ...) {
    #' Converts `SingleR` results to standard format
    #'
    #' @param results `DFrame` of `SingleR` annotation results
    #' @param convert_to `character` that represents output data format.
    #' Options: <labels>
    #'
    #' @returns `SingleR` results in `convert_to` format

    switch(convert_to,
      "labels" = results$labels
    )
  },
  preprocess_expr = function(expr_data, ...) {
    args <- list(
      list(...),
      expr_type = "logcounts"
    )

    Seurat::GetAssayData(expr_data, args$expr_type)
  },
  preprocess_ref = function(ref_data, ...) {
    args <- list(
      list(...),
      ref_type = "logcounts"
    )

    Seurat::GetAssayData(ref_data, args$ref_type)
  }
)
