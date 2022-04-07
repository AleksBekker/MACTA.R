
#--- Load general packages ---#

#--- Functions for annotation tools ---#

annotate_SingleR <- function(expr_data, ref_data, ...) {
  return(SingleR::SingleR(test = expr_data,
                          ref = ref_data,
                          assay.type.test = 1))
}

#--- Main annotation functions ---#

select_annot_funcs <- function(annot_type, tools.to_use,
  tools.avilable = list(
    ref = list('SingleR' = annotate_SingleR, 'test' = print),
    marker = list()
  )) {
  
  if (identical(tools.to_use, '*')) {
    return(tools.avilable[[annot_type]])
  } else {
    return(tools.avilable[[annot_type]][tools.to_use])
  }
}

annotate <- function(expr_data, ref_data, annot_type, annot_tools = '*', ...) {
  tools_funcs <- select_annot_funcs(annot_type, annot_tools)
  
  for (tools_func in tools_funcs) {
    tools_func(expr_data, ref_data, ...)
  }
}

