
library(methods)

library(magrittr)

# `CTAInterface` class declaration ---------------------------------------------

CTAInterface <- setClass(

    # Name
    "CTAInterface",

    # Slots
    slots = c(
        annotate_func = "function",
        convert_func = "function"
    ),

    # Default values
    prototype = list(
        annotate_func = function(expr_data, ref_data, labels_col, ...) {
            stderr("`annotation_func` must be implemented by subclasses")
        },
        convert_func = function(results, convert_to, ...) {
            stderr("`conversion_func` must be implemented by subclasses")
        }
    )
)

# `annotate_func` getters and setters ------------------------------------------

setGeneric("annotate_func", function(x) standardGeneric("annotate_func"))
setGeneric("annotate_func<-", function(x, v) standardGeneric("annotate_func<-"))

# TODO: add documentation
setMethod("annotate_func", "CTAInterface", function(x) x@annotate_func)
setMethod("annotate_func<-", "CTAInterface", function(x, v) {
    x@annotate_func <- value
})

# `convert_func` getters and setters -------------------------------------------

setGeneric("convert_func", function(x) standardGeneric("convert_func"))
setGeneric("convert_func<-", function(x, v) standardGeneric("convert_func<-"))

# TODO: add documentation
setMethod("convert_func", "CTAInterface", function(x) x@convert_func)
setMethod("convert_func<-", "CTAInterface", function(x, v) {
    x@convert_func <- v
})

# `run_full` function implementation -------------------------------------------

setGeneric(
    "run_full",
    function(cta_tool, expr_data, ref_data, labels_col, convert_to, ...) {
        standardGeneric("run_full")
    }
)

setMethod(
    "run_full",
    definition = function(cta_interface, expr_data, ref_data, labels_col,
                          convert_to, ...) {
        annotate_func(cta_interface)(expr_data, ref_data, labels_col, ...) %>%
            convert_func(cta_interface)(convert_to, ...) %>%
            return()
    }
)