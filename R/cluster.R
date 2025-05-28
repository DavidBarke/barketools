#' Cluster strings
#'
#' @param s Character vector of strings to be clustered.
#' @param h Height at which to cut the tree.
#'
#' @returns An integer vector the same length as `s` specifying the cluster
#' assigned to each string.
#'
#' @export
cluster <- function(
    s,
    h = 250,
    dist_method = "lcs",
    cluster_method = "ward.D"
) {
  if (length(s) == 1) return(1)
  sdm <- 1 - stringdist::stringsimmatrix(s, method = dist_method)
  tree <- hclust(as.dist(sdm), method = cluster_method)
  cutree(tree, h = h)
}



#' Assess cluster
#'
#' @export
assess_cluster <- function(pta_cluster, assignee_id = NULL) {
  pta_cluster_filtered <- if (is.null(assignee_id)) {
    pta_cluster |>
      filter(assignee_id == sample(assignee_id |> unique(), 1))
  } else {
    pta_cluster |>
      filter(assignee_id == {{assignee_id}})
  }

  pta_cluster_filtered |>
    group_by(cluster) |>
    mutate(n_patents_cluster = sum(n_patents)) |>
    ungroup() |>
    relocate(n_patents_cluster, .after = n_patents) |>
    arrange(cluster, desc(n_patents))
}
