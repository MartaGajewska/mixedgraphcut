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
  image_graph <- conv_image_to_graph(edges_caps)

  # Create image partitioning using max flow algorithm
  partitioning <- calc_partitioning(image_graph)

  return(partitioning)
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
#' @export
#'
#' @examples NULL
calc_edges_caps <- function(input_image_df, limits_object, limits_background, method, n_dist){
  # Add pixel tagging to mark pixels selected by the user as object & background
  input_image_df <- add_user_tagging(input_image_df, limits_object, limits_background)

  # Create a list with parameters that will be used in the calculations
  params <- get_params_list(input_image_df, method, n_dist)

  # Calculate probabilities that a pixel color is coming from the same distribution
  # as pixels selected as object / as background
  vertices_probs <- calc_vertices_probs(input_image_df, method, params$object_params, params$background_params)

  # Calculate capacities of the edges connecting pixels with source, sink and neighbours
  source_edges_caps       <- calc_source_edges_caps(vertices_probs)
  sink_edges_caps         <- calc_sink_edges_caps(vertices_probs)
  neighborhood_edges_caps <- calc_neighborhood_edges_caps(vertices_probs, params)

  # Combine edges connecting pixels with source, sink and neighbours
  edges_caps <- data.table::rbindlist(list(source_edges_caps, sink_edges_caps, neighborhood_edges_caps))

  return(edges_caps)
}


#' Adds pixel tagging to mark pixels selected by the user as object & background
#'
#' @param input_image_df df storing image in a specific format
#' @param limits_object list with coordinates of area (rectangle) selected as part of the object
#' @param limits_background list with coordinates of area (rectangle) selected as part of the background
#'
#' @return df with additional column named user_tagging
#' @export
#'
#' @examples NULL
add_user_tagging <- function(input_image_df, limits_object, limits_background) {

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
              n_dist = n_dist,
              total_smoothness = 3,
              similarity_smoothness = 3,
              neigh_coef = 1
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
#' @return named list with parameters of estimated distribution
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

    tryCatch({
      mixmdl <-  mixtools::mvnormalmixEM(selected_pixels %>% as.data.frame(), k = n_dist %>% as.numeric())
      params <- list(means = mixmdl$mu, vars = mixmdl$sigma, lambdas = mixmdl$lambda)
      return(params)
    },
    error = function(e) {
      message('Singluar matrix:', e)
      showModal(
        modalDialog(
          div(paste0("In the process of estimating ", type, " distribution, a singular matrix was obtained"))
          ,easyClose = TRUE)
      )
      return(NULL)
    }
    )
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


#' Calculates probabilities that a pixel color is coming from the same
#' distribution as pixels selected as object / as background
#'
#' @param input_image_df df storing image in a specific format
#' @param method selected method of estimating object and background color distributions
#' @param object_params list with parameters of the estimated object distribution
#' @param background_params list with parameters of the estimated background distribution
#'
#' @return df with additional columns named dnorm_object & dnorm_background
#' @export
#'
#' @examples NULL
calc_vertices_probs <- function(input_image_df, method, object_params, background_params){

  # TODO: add handling of b-w pictures (first, check if current version needs any special handling anyway)
  # if(sum(object_params$var!=object_params$var[1,1])==0){
  #   # All color channels are identical
  #   input_image_df <- input_image_df %>%
  #     mutate(dnorm_object = dnorm(node_value_cc_1,
  #                                 mean = object_params$means[1],
  #                                 sd = object_params$var[1,1]),
  #            dnorm_background = dnorm(node_value_cc_1,
  #                                     mean = background_params$means[1],
  #                                     sd = background_params$var[1,1]))
  # } else {

  # Different color channels
  if (method == "regular") {
    # Single multivariate normal distribution
    input_image_df <- input_image_df %>%
      mutate(dnorm_object =
               mvtnorm::dmvnorm(input_image_df %>% select(starts_with("node_value_")),
                                mean  = object_params$mean,
                                sigma = object_params$var),
             dnorm_background =
               mvtnorm::dmvnorm(input_image_df %>% select(starts_with("node_value_")),
                                mean  = background_params$mean,
                                sigma = background_params$var))
  } else {
    # Gaussian mixture
    input_image_df <- input_image_df %>%
      mutate(dnorm_object =
               purrr::map_dfc(1:length(object_params$lambdas),
                              function(i) object_params$lambda[i] * mvtnorm::dmvnorm(
                                input_image_df %>% select(starts_with("node_value_")),
                                mean = object_params$means[[i]],
                                sigma = object_params$vars[[i]])) %>% rowSums(),
             dnorm_background =
               purrr::map_dfc(1:length(background_params$lambdas),
                              function(i) background_params$lambda[i] * mvtnorm::dmvnorm(
                                input_image_df %>% select(starts_with("node_value_")),
                                mean = background_params$means[[i]],
                                sigma = background_params$vars[[i]])) %>% rowSums())
  }


  return(input_image_df)
}


#' Calculates capacities of edges connecting source vertex and pixel vertices
#'
#' @param vertices_probs df with image information and dnorm_object & dnorm_background columns
#'
#' @return df with from & to columns storing node ids, and capacities of those edges
#' @export
#'
#' @examples NULL
calc_source_edges_caps <- function(vertices_probs){

  source_edges_caps <- vertices_probs %>%
    # 1 is the reserved node number for a special vertex - source
    mutate(from = 1) %>%
    rename(`to` = `node_id`) %>%
    # TODO check if -log(x) or x
    # mutate(capacity = -log(dnorm_object)) %>%
    mutate(capacity = dnorm_object) %>%
    # change capacity for selected pixels to a high constant to keep user constrain
    mutate(capacity = ifelse(is.na(user_tagging) | user_tagging != "object", capacity, 1000000)) %>%
    select(from, to, capacity)

  return(source_edges_caps)
}


#' Calculates capacities of edges connecting sink vertex and pixel vertices
#'
#' @param vertices_probs df with image information and dnorm_object & dnorm_background columns
#'
#' @return df with from & to columns storing node ids, and capacities of those edges
#' @export
#'
#' @examples NULL
calc_sink_edges_caps <- function(vertices_probs){

  sink_edges_caps <- vertices_probs %>%
    # 2 is the reserved node number for a special vertex - sink
    rename(`from` = `node_id`) %>%
    mutate(to = 2) %>%
    # TODO check if -log(x) or x
    # mutate(capacity = -log(dnorm_background)) %>%
    mutate(capacity = dnorm_background) %>%
    # change capacity for selected pixels to a high constant to keep user constrain
    mutate(capacity = ifelse(is.na(user_tagging) | user_tagging != "background", capacity, 1000000)) %>%
    select(from, to, capacity)

  return(sink_edges_caps)
}


#' Calculates capacities of edges connecting pixel vertices
#'
#' @param vertices_probs df with image information and dnorm_object & dnorm_background columns
#' @param params named list with parameters
#'
#' @return df with from & to columns storing node ids, and capacities of those edges
#' @export
#'
#' @examples NULL
calc_neighborhood_edges_caps <- function(vertices_probs, params){

  # Create a df with neighbouring (4) pixels
  # Each pair is in the subset twice (x -> y & y -> x)

  # calculate neighbor coordinates
  neighborhood_df <- dplyr::full_join(
    vertices_probs,
    # neighbors = same column, adjacent row OR same row, adjacent column
    data.frame(mod_col = c(1, -1, 0, 0), mod_row = c(0, 0, 1, -1)),
    by = character()) %>%
    mutate(ngb_column = column + mod_col, ngb_row = row + mod_row)

  # find neighbor pixel value
  neighborhood_merged <-
    full_join(
      neighborhood_df,
      vertices_probs,
      by = c('ngb_column' = 'column', 'ngb_row' = 'row'),
      all.x = TRUE
    ) %>%
    # remove rows with ngb_column or ngb_row outside picture boarders
    filter(!is.na(`node_id.y`))

  # TODO add comments explaining the formula
  # define constast coef
  color_diff_sq <- neighborhood_merged %>%
    mutate(
      color_diff_sq =
        (node_value_cc_1.x - node_value_cc_1.y) ^ 2 +
        (node_value_cc_2.x - node_value_cc_2.y) ^ 2 +
        (node_value_cc_3.x - node_value_cc_3.y) ^ 2
    ) %>%
    pull(color_diff_sq)

  # TODO add comments explaining the formula
  # TODO check -  should we add sqrt somewhere? it's not in the source formula, but would make sense
  contrast <- 1/(2*sum(color_diff_sq)/length(color_diff_sq))

  # TODO add comments explaining the formula
  # calc node capacity
  neighborhood_edges_caps <- neighborhood_merged %>%
    mutate(capacity= (params$total_smoothness + params$similarity_smoothness*exp(-(
      (node_value_cc_1.x - node_value_cc_1.y)^2 +
        (node_value_cc_2.x - node_value_cc_2.y)^2 +
        (node_value_cc_3.x - node_value_cc_3.y)^2
    ))*contrast)*params$neigh_coef
  ) %>%
    select(node_id.x, node_id.y, capacity)

  colnames(neighborhood_edges_caps) <-
           c("from", "to", "capacity")

  return(neighborhood_edges_caps)
}

#' Converts data frame with edges and capacities to igraph format
#'
#' @param edges_caps df with edges and capacities
#'
#' @return igraph object
#' @export
#'
#' @examples
#' NULL
conv_image_to_graph <- function(edges_caps) {
  image_graph <-
    igraph::graph_from_data_frame(as.data.frame(edges_caps))

  return(image_graph)
}

# TODO add documentation
calc_partitioning <- function(image_graph) {
  partitioning <- igraph::max_flow(
    image_graph,
    source = igraph::V(image_graph)["1"],
    target = igraph::V(image_graph)["2"]
  )

  return(partitioning)
}
