# find_unique_labels_per_column function implementation ------------------------

#' Find all of the unique values in the columns of a data.frame
#'
#' @param df A data.frame to be analyzed
#'
#' @return A list of colnames -> vector of unique values
find_unique_labels_per_column <- function(df) {
  lapply(df, function(x) levels(factor(x)))
}

# find_unique_labels_in_set function implementation ----------------------------

#' Find all of the unique values in the entirety of a data.frame
#'
#' @param df A data.frame to be analyzed
#'
#' @return A vector of unique values
find_unique_labels_in_set <- function(df) {
  levels(factor(unlist(find_unique_labels_per_column(df))))
}

# find_label_counts_per_column function implementation -------------------------

find_label_counts_per_column <- function(df) {
  sapply(
    colnames(df),
    function(x) table(df[x])
  )
}

# find_label_counts_per_cell function implementation ---------------------------

#' Find the amount of
#' @export
find_popular_label_per_cell <- function(labeled_cells, weights = NULL) {
  # TODO rename function
  cell_labels <- find_unique_labels_in_set(labeled_cells)

  if (is.null(weights)) {
    weights <- rep(1, length(labeled_cells))
  }

  if (is.null(names(weights))) {
    names(weights) <- cell_labels
  } else {
    weights <- weights[cell_labels]
  }

  apply(labeled_cells, 1, function(x) {
    names(
      which.max(
        apply(
          sapply(
            cell_labels,
            function(y) (x == y) * weights
          ),
          2,
          sum
        )
      )
    )
  })
}

#' @export
find_label_counts_per_cluster <- function(labeled_cells, cluster_vector,
                                          weights = NULL) {

  # TODO: check this function
  labeled_counts <- find_label_counts_per_cell(labeled_cells, weights)

  unique_labels <- as.character(levels(factor(labeled_counts)))
  cluster_names <- as.character(levels(factor(cluster_vector)))

  tally <- matrix(
    0,
    nrow = length(unique_labels),
    ncol = length(cluster_names)
  )
  tally <- as.data.frame(tally)
  rownames(tally) <- unique_labels
  colnames(tally) <- cluster_names

  print(tally)

  # sapply(
  #     names(cluster_vector),
  #     function(x) {
  #         tally[labeled_counts[x], cluster_vector[x]] <-
  #             tally[labeled_counts[x], cluster_vector[x]] + 1
  #     }
  # )

  looping_df <- data.frame(labels = labeled_counts, clusters = cluster_vector)
  for (i in seq_len(nrow(looping_df))) {
    tally[looping_df[i, "labels"], looping_df[i, "clusters"]] <-
    tally[looping_df[i, "labels"], looping_df[i, "clusters"]] + 1
  }

  tally
}
