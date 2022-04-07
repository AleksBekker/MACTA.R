
# Load general packages --------------------------------------------------------

library(magrittr)

library(docstring)
library(methods)

# Util functions ---------------------------------------------------------------

convert_confidence_to_ranks(confidence_mat, decreasing = TRUE) {
  #' Makes a matrix of ranks of confidence (per column) from a confidence matrix
  #' 
  #' @param confidence_mat `matrix` of confidence scores
  #' @param decreasing Boolean that determines whether the directionality of the ranking
  #' 
  #' @return Matrix of the ranks of the confidence scores against the columns they are in 
  
  sapply(1:ncol(confidence_mat),
         function(i) confidence_mat[, i] %>% 
           order(decreasing = decreasing) %>% 
           order) %>% return
}

# SingleR Functions ------------------------------------------------------------

annotate_SingleR <- function(expr_data, ref_data, ...) {
  #' Runs a `SingleR` analysis using the standard 'API' for this project
  #' 
  #' @param expr_data `Seurat` object that holds a scRNAseq experiment's data
  #' @param ref_data `Seurat` object that holds the scRNAseq reference
  #' 
  #' @return `DFrame` result of the `SingleR` method
  
  SingleR::SingleR(test = expr_data,
                   ref = ref_data,
                   assay.type.test = 1) %>% return
}

convert_SingleR <- function(res, convert_to) {
  #' Converts the `DFrame` results of a `SingleR` analysis using the standard 'API' for this project
  #' 
  #' @param res `DFrame` results of a `SingleR` analysis
  #' @param convert_to `character` the type of result needed <'labels'/'ranks'>
  #' 
  #' @return data structure containing the converted data
  
  switch (convert_to,
    'labels' = res$labels,
    'ranks' = convert_confidence_to_ranks(res$scores),
  ) %>% return
}

# Tool information -------------------------------------------------------------

tools_info.default <- list(
  ref = list(
    SingleR = list(tool_name = 'SingleR',
                   annotation_func = annotate_SingleR,
                   conversion_func = convert_SingleR),
  ),
  marker = list()
)

# Main annotation functions ----------------------------------------------------

select_tools_info <- function(annot_type, tools.to_use, 
                              tools_info.available = tools_info.default) {
  #' Interpret `annotate` function arguments to determine which tools need to be used
  #' 
  #' @param annot_type Type of CTA to perform <"ref"/"marker">
  #' @param annot_tools Which tools of type `annot_type` to use. '*' selects all tools
  #' @param tools_info.available A list of tool information that has the same structure as `tools_info.defaults`
  #' 
  #' @return `list` of `tool_info`s to be used in further analysis
  
  tools_info = tools_info.available[[annot_type]]
  
  if (!identical(tools.to_use, '*')) {
    tools_info <- tools_info[tools.to_use]
  }
  
  return(tools_info)
}


annotate <- function(expr_data, ref_data, annot_type, annot_tools = '*', 
                     tool_info = tool_info.default, result_type = 'labels', ...) {
  #' Auto-annotate a sample
  #' 
  #' @param expr_data `Seurat` object that holds a scRNAseq experiment's data
  #' @param ref_data `Seurat` object that holds the scRNAseq reference or markers (depending on `annot_type`)
  #' @param annot_type `character` describing the type of CTA to perform <"ref"/"marker">
  #' @param annot_tools `character` holding method names of tools to use. '*' selects all tools for proper `annot_type`
  #' @param result_type `character` naming the type of output collected from each tool
  #'   
  #' @return TBD
  
  tools_info <- select_tools_info(annot_type, annot_tools, 
                                  tool_info.available = tool_info)
  
  lapply(select_tools_info, 
         function(tool) tool$annotation_func(expr_data, ref_data, ...) %>% 
           tool$conversion_func(convert_to = result_type))
}

