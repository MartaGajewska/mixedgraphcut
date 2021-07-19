
#' Performs an image segmentation
#'
#' @param image a 3-dimensional input image (the range of the pixel values should be preferably in the range 0 to 255)
#' @param method a character string specifying the variation of graph cut method. It can be either "regular", "mixed or "mixed_bic"
#' @param k
#'
#' @return
#' @export
#'
#' @examples
cut_image <- function(image, method, k = NULL) {
  # prepare_image TODO
  selected_areas <- select_areas(image)
  segmentation <- case_when(
    method = "regular" ~ graphcut_regular(image),
    method = "mixed" ~ graphcut_mixed(image, k),
    method = "mixed_bic" ~ graphcut_mixed_bic(image)
  )
  return(segmentation)
}

graphcut_regular <- function(image) {

}

graphcut_mixed <- function(image, k) {

}

graphcut_mixed_bic <- function(image) {

}
