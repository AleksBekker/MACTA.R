# Create garnett interface -----------------------------------------------------

garnett_interface <- CTAInterface(
  annotate = function(expr_data, ref_data, ...) {},
  convert = function(results, convert_to, ...) {
    switch(convert_to,
      "labels" = NULL
    )
  }
)
