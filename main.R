library(magrittr)
library(optparse)

source("annotation/annotate.R")

parse_cli_args <- function() {
  option_list <- list(
    make_option(
      c("-e", "--expr_data"),
      type = "character",
      dest = "expr_data_path",
      help = "path to experiment data in h5 seurat file"
    ),
    make_option(
      c("-r", "--ref_data"),
      type = "character",
      dest = "ref_data_path",
      help = "path to experiment data in h5 seurat file"
    ),
    make_option(
      c("-l", "--labels"),
      type = "character",
      help = "string name of column in `ref_data`"
    ),
    make_option(
      c("-t", "--tests"),
      type = "character",
      dest = "annot_test_unsplit",
      help = "path to experiment data in h5 seurat file"
    ),
    make_option(
      "--tools",
      type = "character",
      dest = "annot_tools",
      default = "*",
      help = paste(
        "list of tools to be used in analysis, delimited by `,`s, "*" for all",
        "[default %default]",
        sep = ""
      )
    ),
    make_option(
      c("-o", "--output"),
      type = "character",
      dest = "output_path",
      help = "path to output file"
    ),
    make_option(
      "--result_type",
      type = "character",
      dest = "convert_to",
      default = "labels",
      help = "type of output to be generated by the pipeline [default %default]"
    )
  )

  option_list %>%
    OptionParser(option_list = .) %>%
    parse_args() %>%
    return()
}

main <- function() {
  source("data/load_SCINA_examples.R")
  annotate(expr_data, as.list(ref_data), "marker")
}

main()
