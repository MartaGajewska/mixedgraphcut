# TODO add supporting functions

#' Creates image partitioning into object and background based on selected image areas
#'
#' @param input_image_df df storing image in a specific format
#' @param limits_object list with coordinates of area (rectangle) selected as part of the object
#' @param limits_background list with coordinates of area (rectangle) selected as part of the background
#' @param method selected method of estimating object and background color distributions
#' @param n_dist number of distributions selected for method "mixed"
#'
#' @return partitioning, two lists of pixels, containing pixels classified as object and as background
#' @export
#'
#' @examples NULL
create_partitioning <- function(input_image_df, limits_object, limits_background, method, n_dist = NULL) {

  # Calculate values that will be associated with each graph edge
  edges_caps <- calc_edges_caps(input_image_df, limits_object, limits_background, method, n_dist)
  # default params in config, maybe option to modify in the app

  # Convert to igraph object
  image_graph <- conv_image_to_graph(image_with_node_values)

  # Create image partitioning using max flow algorithm
  partitioning <-
    igraph::max_flow(
      image_graph,
      source = igraph::V(image_graph)["1"],
      target = igraph::V(image_graph)["2"]
    )
}

#' Calculates capacities of graph edges
#'
#' @param input_image_df df storing image in a specific format
#' @param limits_object list with coordinates of area (rectangle) selected as part of the object
#' @param limits_background list with coordinates of area (rectangle) selected as part of the background
#' @param method selected method of estimating object and background color distributions
#' @param n_dist number of distributions selected for method "mixed"
#'
#' @return df storing edge capacities with each edge in a separate row
#'
#' @examples NULL
calc_edges_caps <- function(input_image_df, limits_object, limits_background, method, n_dist){

  # Create a list with parameters that will be used in the calculations
  params <- get_params_list(input_image_df, limits_object, limits_background, method, n_dist)

  # Calculate probabilities that a pixel color is coming from the same distribution
  # as pixels selected as object / as background
  vertices_probs <- calc_vertices_probs(input_image_df, method, params)

  # Calculate capacities of the edges connecting pixels with source, sink and neighbours
  sources_edges_caps      <- calc_sources_edges_caps(vertices_probs)
  sink_edges_caps         <- calc_sink_edges_caps(vertices_probs)
  neighborhood_edges_caps <- calc_neighborhood_edges_caps(vertices_probs, params)

  # Combine edges connecting pixels with source, sink and neighbours
  edges_caps <- rbindlist(list(sources_edges_caps, sink_edges_caps, neighborhood_edges_caps))

  return(edges_caps)
}

# TODO add docs
get_params_list <- function(input_image_df, limits_object, limits_background, method, n_dist){

  # Get object and background parameters from areas selected by user
  object_params <- calc_params_selection(test_image, "object", mode, k_object)
  background_params <- calc_params_selection(test_image, "background", mode, k_background)

  # TODO create a list with params from above, from args and from config
}

# TODO add docs
calc_params_selection <- function(test_image, type, mode, k = NA) {
  # TODO fill function

  # x <- case_when(
  #   method = "regular" ~ estimate_regular(image),
  #   method = "mixed" ~ estimate_mixed(image, k),
  #   method = "mixed_bic" ~ estimate_mixed_bic(image)
  # )
  #
}
