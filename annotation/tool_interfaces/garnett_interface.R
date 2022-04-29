# Load libraries ---------------------------------------------------------------
library(garnett)

library(magrittr)

source("annotation/tool_interfaces/cta_interface.R")

# Create garnett interface -----------------------------------------------------

garnett_interface <- CTAInterface(
  
  annotate = function(expr_data, ref_data, ...) {},
  convert = function(results, convert_to, ...) {
    switch(
      convert_to,
      "labels" = NULL
    ) %>% return()
  }
)