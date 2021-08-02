get_image <- function(image_path) {
  # Resize to max. 120 x 120 pixels
  image <- imager::load.image(image_path)

  dims <- image %>% dim %>% .[1:2]
  new_dims <- round(120*dims/max(dims))

  image <- image %>%
    imager::resize(new_dims[1], new_dims[2])

  return(image)
}

conv_image_to_df <- function(image){

  # convert to graph
  image_df <-
    image %>%
    as.data.frame()

  if (!("cc" %in% colnames(image_df))) {
    image_df <- image_df %>%
      merge(data.frame(cc = c(1, 2, 3)))
  }

  image_df <-
    image_df %>%
    tidyr::pivot_wider(names_from = cc, names_prefix = "node_value_cc_") %>%
    mutate(rgb_value = rgb(red = node_value_cc_1, green = node_value_cc_2, blue = node_value_cc_3)) %>%
    rename(column = x,
           row = y
    ) %>%
    # Node 1 = source, node 2 = sink
    mutate(node_id = 2+row_number())

  # wide histogram
  # image_df$node_value <- (image_df$node_value - min(image_df$node_value))/(max(image_df$node_value)-min(image_df$node_value))
  # image_df %>%
  #   group_by(cc) %>%
  #   mutate(node_value  =
  #            (node_value - min(node_value))/(max(node_value)-min(node_value))) %>%
  #   glimpse

  return(image_df)
}
