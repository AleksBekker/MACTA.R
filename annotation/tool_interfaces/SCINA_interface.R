
library(preprocessCore)
library(SCINA)

library(magrittr)

source("annotation/tool_interfaces/cta_interface.R")

SCINA_interface <- CTAInterface(
  annotate = function(expr_data, ref_data, ...) {
    SCINA(expr_data, ref_data) %>% return()
  },
  convert = function(results, convert_to, ...) {
    switch(convert_to,
      "labels" = results$cell_labels
    ) %>% return()
  },
  preprocess_expr = function(expr_data, ...) {
    args <- list(
      expr_type = "logcounts"
    ) %>% c(list(...), .)

    if (args$expr_type == "counts") {
      exp[] <- expr_data %>%
        as.matrix() %>%
        log(. + 1) %>%
        normalize.quantiles()
      return(exp)
    }

    return(expr_data)
  }
)
