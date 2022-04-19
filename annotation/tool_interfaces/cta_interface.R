
library(methods)

library(magrittr)

# `CTAInterface` class declaration ---------------------------------------------

CTAInterface <- setClass(

  # Name
  "CTAInterface",

  # Slots
  slots = c(
    annotate_func = "function",
    convert_func = "function"
  ),

  # Default values
  prototype = list(
    annotate_func = function(expr_data, ref_data, labels, 
                             expr_type = "logcounts", ref_type = "logcounts", 
                             ...) {
      stderr("`annotation_func` must be implemented by subclasses")
    },
    convert_func = function(results, convert_to, ...) {
      stderr("`conversion_func` must be implemented by subclasses")
    }
  )
)

# `annotate_func` and `convert_func` getters -----------------------------------

annotate_func <- function(cta_interface) cta_interface@annotate_func
convert_func <- function(cta_interface) cta_interface@convert_func

# `run_full` function implementation -------------------------------------------


run_full <- function(cta_interface, expr_data, ref_data, labels,
                     convert_to, expr_type = "logcounts",
                     ref_type = "logcounts", ...) {
  #' Runs `annotate_func` then `convert_func` on a data set
  #'
  #' @param expr_data experimental data, on which CTA is performed
  #' @param ref_data reference/marker data for the CTA anaysis
  #' @param labels string name of the labels column in `ref_data`
  #' @param annot_type which type of CTA to perform <marker/ref>
  #' @param annot_tools `character` of test names to perform, '*' selects all
  #' @param convert_to output data format
  #' @param tool_interfaces list of interfaces the program will use
  #'
  #' @returns a list of  test_name -> test_results

  expr_data = preprocess_expr_func(cta_interface)(expr_data, ...)
  ref_data = preprocess_ref_func(cta_interface)(ref_data, ...)
  labels = preprocess_labels_func(cta_interface)(labels, ref_data, ...)
  convert_to = preprocess_convert_to_func(cta_interface)(convert_to, ...)
  
  annotate_func(cta_interface)(expr_data, ref_data, labels, 
                               expr_type = expr_type, ref_type = ref_type,
                               ...) %>%
    convert_func(cta_interface)(convert_to, ...) %>%
    return()
}
