# ---- REQUIREMENT CLASS DEFINITION ----

Requirement <- function(value, validator) {
    obj <- list(value = value, validator = validator)
    attr(obj, "class") <- "Requirement"
    obj
}

# ---- COMPATIBILITY CHECKERS ----

#' @export
is_compatible <- function(requirement, other_value) {
    requirement$validator(requirement$value, other_value)
}

#' @export
are_compatible <- function(requirement, other_values) {
    all(sapply(other_values, function(x) is_compatible(requirement, x)))
}

# ---- VALIDATOR FUNCTIONS ----

#' @export
is_validator <- function(value, other_value) {
    is(other_value, value)
}

#' @export
strict_validator <- function(value, other_value) {
    value == other_value
}

#' @export
contains_validator <- function(value, other_value) {
    other_value %in% value
}

#' @export
strict_list_type_validator <- function(values, other_value) {
    names_rq <- Requirement(values[1], is_validator)
    values_rq <- Requirement(values[2], is_validator)

    is(other_value, list) &
        are_compatible(names_rq, names(other_value)) &
        are_compatible(values_rq, unlist(other_value))
}
