# ---- REQUIREMENT_LIST CLASS DEFINITION ----

methods::setClass(

    # NAME
    "RequirementList",

    # SLOTS
    slots = c(
        requirements = "list"
    )
)

# ---- REQUIREMENT_LIST CLASS METHODS ----

#' @export
RequirementList <- function(list_of_rqs = list(), ...) {
    new("RequirementList", list(
        list(...),
        list_of_rqs
    ))
}

requirements <- function(rq_list) rq_list@requirements

# ---- COMPATIBILITY CHECKERS ----

#' @export
all_compatibility_checker <- function(rq_list, values, compatibility_func) {
    all(
        sapply(
            names(values),
            function(x) compatibility_func(requirements(rq_list)[x], values[x])
        )
    )
}

#' @export
all_is_compatible <- function(rq_list, values) {
    all_compatibility_checker(rq_list, values, is_compatible)
}

#' @export
all_are_compatible <- function(rq_list, values) {
    all_compatibility_checker(rq_list, values, are_compatible)
}
