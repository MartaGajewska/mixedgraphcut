# TODO finish documentation
# TODO add supporting functions

#' Creates image partitioning
#'
#' @param input_image_df
#' @param limits_object
#' @param limits_background
#'
#' @return
#' @export
#'
#' @examples NULL
create_partitioning <- function(input_image_df, limits_object, limits_background) {

  # Calculate values that will be associated with each graph edge
  # (used to be called calc_node_values)
  image_with_node_values <- calc_edges_values(input_image_df, limits_object, limits_background)

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
