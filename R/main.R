#' @title The main menu for the mixedgraphcut package
#'
#' @description This function opens a Shiny app with a menu that will allow the user to run the different image segmentations.
#'
#' @details Run this function with no arguments to start the main menu (a Shiny app) for interactive image segmentation.
#' @examples
#' \dontrun{run_segmentation_app()}
#' @author Marta Gajewska
#' @import shiny
#' @export
run_segmentation_app <- function(){

  appDir <- system.file("mixedgraphcut", package = "mixedgraphcut") #get directory for main menu app
  shiny::runApp(appDir = appDir, launch.browser = TRUE) #run main menu app

  print('*************************************************')
  print('I hope you got a segmentation you were looking for!')
  print('*************************************************')

}


.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the mixedgraphcut package. Type run_segmentation_app() to get started.")
}
