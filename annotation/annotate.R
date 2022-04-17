
# Import custom annotation tool interfaces -------------------------------------

source("annotate/tool_interfaces/SingleR_interface.R")

# `annotate` function implementation -------------------------------------------

annotate <- function(expr_data, ref_data, labels_col,
                     annot_types, annot_tools = "*", convert_to = "labels",
                     tool_interfaces = list(
                         ref = list(
                             "SingleR" = SingleR_interface,
                         ),
                         marker = list(),
                     ), ...) {
    #' Performs multi-tool auto-CTA
    #'
    #' @param expr_data experimental data, on which CTA is performed
    #' @param ref_data reference/marker data for the CTA anaysis
    #' @param labels_col string name of the labels column in `ref_data`
    #' @param annot_types which type of CTA to perform <marker/ref>
    #' @param annot_tools `character` of test names to perform, '*' selects all
    #' @param convert_to output data format
    #' @param tool_interfaces list of interfaces the program will use
    #'
    #' @returns a list of  test_name -> test_results
}