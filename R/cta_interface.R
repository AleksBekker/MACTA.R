# `CTAInterface` class declaration ---------------------------------------------

#' @include requirement_list.R
NULL

#' An S4 class to represent an interface to a cell type annotation tool
#'
#' @slot rq_list `RequirementList` for interface parameters
#' @slot annotate function that performs annotation
#' @slot convert function that converts results from `annotate` to the format
#' dictated by the user
#' @slot preprocess_expr function that transforms expression data into the
#' format required by an annotation tool
#' @slot preprocess_ref fucntion that transforms reference data into the format
#' required by an annotation tool
#' @export
#'
setClass("CTAInterface",

  # Slots
  slots = c(
    rq_list = "RequirementList",
    annotate = "function",
    convert = "function",
    preprocess_expr = "function",
    preprocess_ref = "function"
  ),

  # Default values
  prototype = list(
    annotate = function(expr_data, ref_data, ...) stop("Not Yet Implemented"),
    convert = function(results, convert_to, ...) stop("Not Yet Implemented"),
    preprocess_expr = function(expr_data, ...) expr_data,
    preprocess_ref = function(ref_data, ...) ref_data
  )
)

# getters ----------------------------------------------------------------------

rq_list <- function(cta) cta@rq_list
annotate_func <- function(cta) cta@annotate
convert_func <- function(cta) cta@convert
preprocess_expr_func <- function(cta) cta@preprocess_expr
preprocess_ref_func <- function(cta) cta@preprocess_ref

# `run_full` function implementation -------------------------------------------

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
#' @export
run_full <- function(cta_interface, expr_data, ref_data, convert_to, ...) {
  expr_data <- preprocess_expr_func(cta_interface)(expr_data, ...)
  ref_data <- preprocess_ref_func(cta_interface)(ref_data, ...)

  annot_results <- annotate_func(cta_interface)(expr_data, ref_data, ...)
  convert_func(cta_interface)(annot_results, convert_to, ...)
}
