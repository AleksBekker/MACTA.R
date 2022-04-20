
library(methods)

library(magrittr)

# `CTAInterface` class declaration ---------------------------------------------

CTAInterface <- setClass(

  # Name
  "CTAInterface",

  # Slots
  slots = c(
    annotate = "function",
    convert = "function",
    preprocess_expr = "function",
    preprocess_ref = "function"
  ),

  # Default values
  prototype = list(
    annotate = function(expr_data, ref_data, ...) stderr("Not Yet Implemented"),
    convert = function(results, convert_to, ...) stderr("Not Yet Implemented"),
    preprocess_expr = function(expr_data, ...) expr_data,
    preprocess_ref = function(ref_data, ...) ref_data
  )
)

# `annotate_func` and `convert_func` getters -----------------------------------

annotate_func <- function(cta) cta@annotate
convert_func <- function(cta) cta@convert
preprocess_expr_func <- function(cta) cta@preprocess_expr
preprocess_ref_func <- function(cta) cta@preprocess_ref

# `run_full` function implementation -------------------------------------------


run_full <- function(cta_interface, expr_data, ref_data, convert_to, ...) {
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

  expr_data <- preprocess_expr_func(cta_interface)(expr_data, ...)
  ref_data <- preprocess_ref_func(cta_interface)(ref_data, ...)

  annotate_func(cta_interface)(expr_data, ref_data, ...) %>%
    convert_func(cta_interface)(convert_to, ...) %>%
    return()
}
