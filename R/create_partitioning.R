#' Creates image partitioning into object and background based on selected image areas
#'
#' @param input_image_df df storing image in a specific format
#' @param limits_object list with coordinates of area (rectangle) selected as part of the object
#' @param limits_background list with coordinates of area (rectangle) selected as part of the background
#' @param method selected method of estimating object and background color distributions
#' @param n_dist list with number of distributions selected for method "mixed" for object & background
#'
#' @return partitioning, two lists of pixels, containing pixels classified as object and as background
#' @export
#'
#' @examples NULL
create_partitioning <- function(input_image_df, limits_object, limits_background, method, n_dist = NULL) {

  # Set seed (there is randomness in distribution estimates)
  set.seed(17)

  # Calculate values that will be associated with each graph edge
  edges_caps <- calc_edges_caps(input_image_df, limits_object, limits_background, method, n_dist)
  # default params in config, maybe option to modify in the app

  # Convert to igraph object
  # TODO add supporting functions
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
#' @param n_dist list with number of distributions selected for method "mixed" for object & background
#'
#' @return df storing edge capacities with each edge in a separate row
#'
#' @examples NULL
calc_edges_caps <- function(input_image_df, limits_object, limits_background, method, n_dist){

  # Add pixel tagging to mark pixels selected by the user as object & background
  input_image_df <- add_user_selections(input_image_df, limits_object, limits_background)

  # Create a list with parameters that will be used in the calculations
  params <- get_params_list(input_image_df, method, n_dist)

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


#' Adds pixel tagging to mark pixels selected by the user as object & background
#'
#' @param input_image_df
#' @param limits_object
#' @param limits_background
#'
#' @return
#' @export
#'
#' @examples
add_user_selections <- function(input_image_df, limits_object, limits_background) {

  input_image_df %>%
    mutate(user_tagging =
              # object
              ifelse(between(column, limits_object$xmin, limits_object$xmax) & between(row, limits_object$ymin, limits_object$ymax), "object",
              # background
              ifelse(between(column, limits_background$xmin, limits_background$xmax) & between(row, limits_background$ymin, limits_background$ymax), "background",
              # other
              NA)))

}




#' Creates a list of parameters for further calculations
#'
#' @param input_image_df df storing image in a specific format
#' @param method selected method of estimating object and background color distributions
#' @param n_dist list with number of distributions selected for method "mixed" for object & background
#'
#' @return named list with parameters
#' @export
#'
#' @examples NULL
get_params_list <- function(input_image_df, method, n_dist){

  # Get object and background parameters from areas selected by the user
  object_params <- calc_params_distribution(input_image_df, "object", method, n_dist$object)
  background_params <- calc_params_distribution(input_image_df, "background", method, n_dist$background)

  # Create a list with params from above, from args and from config
  return(list(object_params = object_params,
              background_params = background_params,
              n_dist = n_dist
              # Additional params to be added when neededlater on
              ))
}

#' Calculates parameters of the estimated distribution
#'
#' @param input_image_df df storing image in a specific format
#' @param type string, "object" or "background"
#' @param method selected method of estimating object and background color distributions
#' @param n_dist number of distributions selected
#'
#' @return
#' @export
#'
#' @examples NULL
calc_params_distribution <- function(input_image_df, type, method, n_dist = NA) {

  # Subset pixels, keep only those selected by the user
  selected_pixels <- input_image_df %>%
    filter(user_tagging == type) %>%
    select(node_value_cc_1, node_value_cc_2, node_value_cc_3)

  # Calculations depend on an estimation method selected
  if (method == "regular") {
    # Single (3-dim) normal distribution, params are mean and variance
    mean <- colMeans(selected_pixels)
    var <- var(selected_pixels)
    params <- list(mean = mean, var = var)
  }

  if (method == "mixed") {
    # Multivariate normal mixture
    # Params are means, variances, and lambdas (e.g. mixing proportions)
    # n_dist - selected number of distributions in a mix
    mixmdl <-  mixtools::mvnormalmixEM(selected_pixels %>% as.data.frame(), k = n_dist)
    params <- list(means = mixmdl$mu, vars = mixmdl$sigma, lambdas = mixmdl$lambda)
  }

  if (method == "mixed_bic") {
    # Calculate multiple estimations of the distribution
    # Then, basing on BIC select the best number of distributions for the mixture
    # TODO: change hardcoded values (1 & 15) to dynamic ones taken from config / params
    mixmdl_mclust <- mclust::Mclust(selected_pixels, G = 1:15, modelNames = "VVV", warn = TRUE)

    sigma <- mixmdl_mclust$parameters$variance$sigma
    sigma <- lapply(seq(dim(sigma)[3]), function(x) sigma[ , , x])
    means <- mixmdl_mclust$parameters$mean %>% as.data.frame() %>% as.list()

    # Params are means, variances, and lambdas (e.g. mixing proportions)
    params <- list(means = means, vars = sigma, lambdas = mixmdl_mclust$parameters$pro)

    # TODO check if it's still the best way to handle that kind of error
    if(is.null(params$lambdas)) stop("This is an error message from bic")
  }

  return(params)
}
