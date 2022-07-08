SCINA_interface <- CTAInterface(
  rq_list = RequirementList(
    annot_type = Requirement("marker", strict_validator)
  ),
  annotate = function(expr_data, ref_data, ...) {
    SCINA::SCINA(expr_data, ref_data)
  },
  convert = function(results, convert_to, ...) {
    switch(convert_to,
      "labels" = results$cell_labels
    )
  },
  preprocess_expr = function(expr_data, ...) {
    args <- list(
      list(...),
      expr_type = "logcounts"
    )

    expr_data <- GetAssayData(expr_data, args$expr_type)

    if (args$expr_type == "counts") {
      expr_data <- log(expr_data + 1)
      expr_data <- preprocessCore::normalize.quantiles(expr_data)
      rownames(expr_data) <- rownames(expr_data)
      colnames(expr_data) <- colnames(expr_data)
    }

    return(expr_data)
  }
)
