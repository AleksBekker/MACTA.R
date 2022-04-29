# Load libraries ---------------------------------------------------------------

suppressWarnings(library(methods))

suppressWarnings(library(magrittr))

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
  #' @param cta_interface a `CTAInterface` object that has implemented 
  #' `annotate` and `convert` functions for a specific tool, according to which 
  #' the annotation process will be done
  #' @param expr_data experimental data, on which CTA is performed
  #' @param ref_data reference/marker data for the CTA anaysis
  #' @param convert_to output data format
  #'
  #' @returns the annotation results from `cta_interface`, formatted to the 
  #' `convert_to` format

  expr_data <- preprocess_expr_func(cta_interface)(expr_data, ...)
  ref_data <- preprocess_ref_func(cta_interface)(ref_data, ...)

  annotate_func(cta_interface)(expr_data, ref_data, ...) %>%
    convert_func(cta_interface)(convert_to, ...) %>%
    return()
}
