#' Loads the image from specified path and resized it uniformly
#'
#' @param image_path path to the image
#' @param max_size specifies maximum number of rows and columns
#'
#' @return object of cimg class, resized to not exceed max_size on any of the dimensions
#' @export
#'
#' @examples NULL
get_image <- function(image_path, max_size = 80) {
  # Load the image from specified path
  image <- imager::load.image(image_path)

  # Resize
  dims <- image %>% dim %>% .[1:2]
  new_dims <- round(max_size*dims/max(dims))

  image <- image %>%
    imager::resize(new_dims[1], new_dims[2])

  return(image)
}

#' Converts cimg image to a df with a specific format
#'
#' @param image object of class cimg
#'
#' @return df, each row representing one pixel
#' @export
#'
#' @examples NULL
conv_image_to_df <- function(image){
  # convert to graph
  image_df <-
    image %>%
    as.data.frame()

    # If the image is represented in a grayscale, set all cc to be identical
  # (to unify format)
  # cc = color channel
  if (!("cc" %in% colnames(image_df))) {
    image_df <- image_df %>%
      merge(data.frame(cc = c(1, 2, 3)))
  }

  image_df <-
    image_df %>%
    # move each cc to separate column
    tidyr::pivot_wider(names_from = cc, names_prefix = "node_value_cc_") %>%
    # add a column with RGB color coding
    mutate(rgb_value = rgb(red = node_value_cc_1, green = node_value_cc_2, blue = node_value_cc_3)) %>%
    rename(column = x,
           row = y
    ) %>%
    # add node numbering, pixels = 3, 4, 5, ...
    # starting from 3, leaving 1 & 2 for special nodes
    # node 1 = source, node 2 = sink
    mutate(node_id = 2 + row_number())

  return(image_df)
}
