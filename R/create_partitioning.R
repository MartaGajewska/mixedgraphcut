# TODO add supporting functions

#' Creates image partitioning into object and background based on selected image areas
#'
#' @param input_image_df df storing image in a specific format
#' @param limits_object list with coordinates of area (rectangle) selected as part of the object
#' @param limits_background list with coordinates of area (rectangle) selected as part of the background
#'
#' @return partitioning, two lists of pixels, containing pixels classified as object and as background
#' @export
#'
#' @examples NULL
create_partitioning <- function(input_image_df, limits_object, limits_background, method, n_dist = NULL) {

  # Calculate values that will be associated with each graph edge
  # (used to be called calc_node_values)
  image_with_node_values <- calc_edges_values(input_image_df, limits_object, limits_background, method, n_dist)
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
