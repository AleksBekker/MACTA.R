
#--- Load general packages ---#

library(magrittr)

#--- Util functions ---#

convert_confidence_to_ranks(confidence_mat, decreasing = TRUE) {
  sapply(1:ncol(confidence_mat),
         function(i) confidence_mat[,i] %>% order(decreasing = decreasing) %>% order) %>%
    return
}

#--- SingleR Functions ---#

annotate_SingleR <- function(expr_data, ref_data, ...) {
  SingleR::SingleR(test = expr_data,
                   ref = ref_data,
                   assay.type.test = 1) %>% return
}

convert_SingleR_to_ranks <- function(res, use_reciprocal = TRUE) {
  convert_confidence_to_ranks(res$scores) %>% return
}

#--- Main annotation functions ---#

select_annot_funcs <- function(annot_type, tools.to_use,
  tools.avilable = list(
    ref = list('SingleR' = annotate_SingleR),
    marker = list()
  )) {
  
  to_return = tools.avilable[[annot_type]]
  
  if (!identical(tools.to_use, '*')) {
    to_return <- to_return[tools.to_use]
  }
  
  return(to_return)
}

# Description:   
# Params:
#   -expr_data (SeuratObject): Experimental data for cells to be labelled
#   -ref_data (SeuratObject): Reference/marker data to be used in the tools
#   -annot_type (character): Type of CTA to perform <"ref"/"marker">
#   -annot_tools (character): Which tools of type `annot_type` to use. '*' selects all tools
# Outputs: TBD
annotate <- function(expr_data, ref_data, annot_type, annot_tools = '*', ...) {
  tools_funcs <- select_annot_funcs(annot_type, annot_tools)
  
  for (tools_func in tools_funcs) {
    tools_func(expr_data, ref_data, ...) # manage returned data
  }
}

