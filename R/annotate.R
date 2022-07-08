# `annotate` function implementation -------------------------------------------

#' Performs multi-tool auto-CTA
#'
#' @param expr_data `matrix` or `Matrix` of experimental data, on which CTA is
#' performed
#' @param ref_data `matrix` or `Matrix` of reference/marker data for the CTA
#' analysis
#' @param labels `character` vector that holds the labels for each row of
#' `ref_data`
#' @param annot_type which type of CTA to perform <marker/ref>
#' @param annot_tools `character` vector of the names of the tools to be used.
#' '*' selects all of the tools in the `annot_type` category
#' @param convert_to `character` describing the format of the output data
#' @param tool_interfaces list of interfaces the program will use
#'
#' @returns a list of  test_name -> test_results
#' @export
annotate <- function(expr_data, ref_data, annot_type, annot_tools = "*",
                     convert_to = "labels", tool_interfaces = list(
                       "SingleR" = SingleR_interface,
                       "SCINA" = SCINA_interface,
                     ), ...) {

  # Merge default arguments with ... arguments
  args <- list(
    list(...),
    expr_type = "logcounts",
    ref_type = "logcounts",
    labels = NULL,
    temp_dir = tempdir(),
  )

  # Validate `annot_type`
  if (!annot_type %in% c("ref", "marker")) {
    stop("Invalid `annot_type`.")
  }

  if (!is(expr_data, Seurat::Seurat)) {
    stop("`expr_data` must be a `Seurat` object")
  }

  use_cluster_labels <- convert_to == "cluster_labels"
  if (use_cluster_labels) {
    convert_to <- "labels"
  }

  # Process `annot_tools`
  if (identical(annot_tools, "*")) {
    annot_tools <- names(tool_interfaces[[annot_type]])
  }

  rq_values <- list(
    "annot_type" = annot_type
  )

  results <- lapply(
    tool_interfaces[annot_tools],
    function(interface) {
      if (all_is_compatible(rq_list(interface), rq_values)) {
        run_full(interface, expr_data, ref_data, convert_to,
          labels = args$labels, expr_type = args$expr_type,
          ref_type = args$ref_type, ...
        )
      }
    }
  )

  if (use_cluster_labels) {
    sapply
  }

  results
}
