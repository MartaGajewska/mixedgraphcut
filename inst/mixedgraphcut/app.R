################################################################################
# This is the Shiny App for the main menu of mixedgraphcut
################################################################################

#TODO: remove redundant packages
library(shiny)
library(rsconnect)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(gt)
# library(downloadthis)
library(webshot)
library(writexl)
# library(shinythemes)
library(bslib)
library(shinyjs)
library(httr)
library(mclust)
library(ggplot2)
# library(shinyWidgets)
library(shinycssloaders)

################################################################################
#Set up some variables, define all as global (the <<- notation)
#name of R package
packagename <<- "mixedgraphcut"
#find path to apps
appdir <<- system.file("appinformation", package = packagename) #find path to apps


################################################################################
# define functions
################################################################################

# get data ----
gdocs_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQliF1fPTXNk6b4cVwbkD7GmYFmyNKkG2GrzYcr21d-C02L61gZZNrk3beBEf95mQ-doWd3MweAyZKH/pub?gid=0&single=true&output=csv"
#read data from url
exercise_data <- read_csv(gdocs_url)

#set dynamic themes
light <- bs_theme(version = 4, bootswatch = "minty")

# simple function that creates app buttons for UI
# specify data frame containing app info and the id of the app
# make_button <- function(at,appid)
# {
#   id = which(at$appid == appid)
#   actionButton(at$appid[id], paste0(at$apptitle[id]), class="mainbutton")
# }


################################################################################
# server function of the app
################################################################################
server <- function(input,output, session) {
# TODO organize server code by module
  vals <- reactiveValues()

  observe({
    if(input$uploadButton)
      show("selectObjectButton")
  })

  observe({
    hide("selectObjectButton")
    hide("selectBackgroundButton")
    hide("runButton")
    if(input$uploadButton) {
      show("selectObjectButton")
      show("selectBackgroundButton")
      show("runButton")
    }
  })


  input_image_df <- eventReactive(input$uploadButton, {
    mixedgraphcut::get_image(input$input_image$datapath) %>%
      mixedgraphcut::conv_image_to_df()
  })

  #show uploaded image ----

  plot_image_df <- function(image_df)({
      ggplot(image_df, aes(column, row)) +
        geom_raster(aes(fill=rgb_value))  +
        scale_y_reverse() +
        scale_fill_identity() +
        theme(legend.position="none") +
        theme_void() +
        coord_fixed(ratio=1)

  })

  output$uploaded_image <- renderPlot({plot_image_df(input_image_df())})

  observe({
    hide("input_n_dist_object")
    hide("input_n_dist_background")
    if(input$input_method == "mixed") {
      show("input_n_dist_object")
      show("input_n_dist_background")
    }
  })
  # get selected parts of the object and background
  observeEvent(input$selectObjectButton, {
    vals$input_object_box <- list(xmin = input$uploaded_image_brush$xmin, xmax = input$uploaded_image_brush$xmax,
                                  ymin = input$uploaded_image_brush$ymin, ymax = input$uploaded_image_brush$ymax)
  })

  observeEvent(input$selectBackgroundButton, {
    vals$input_background_box <- list(xmin = input$uploaded_image_brush$xmin, xmax = input$uploaded_image_brush$xmax,
                                      ymin = input$uploaded_image_brush$ymin, ymax = input$uploaded_image_brush$ymax)
  })

  # run segmentation and display results
  segmented_image <- eventReactive(input$runButton, {
    # testing
    limits_object <- lapply(vals$input_object_box, round)
    limits_background <- lapply(vals$input_background_box, round)

    # full version - in progress
    input_image_df <- input_image_df()
    image_partitioning <- mixedgraphcut::create_partitioning(input_image_df, limits_object, limits_background, input$input_method, list(object = input$input_n_dist_object, background = input$input_n_dist_background))

    # temporary output  - testing
    # paste0("obj, min: ", limits_object$xmin, ", max: ", limits_object$xmax,
    #        ",   bcg, min: ", limits_background$xmin, ", max: ", limits_background$xmax)
    #
    results_plot <-
      ggplot() +
      geom_raster(data = input_image_df %>%
                    filter(column != 1 ),
                  aes(column-1, row, fill=rgb_value), hjust = 0.5)  +
      geom_point(data = input_image_df %>%
                   filter(node_id %in% image_partitioning$partition2) %>%
                   filter(column != max(input_image_df$column)),
                 aes(column, row), color = "black", alpha = 0.5, size = 2) +
      geom_point(data = input_image_df %>%
                   filter(node_id %in% image_partitioning$partition1) %>%
                   filter(column != max(input_image_df$column)),
                 aes(column, row), color = "white", alpha = 0.5, size = 2) +
      scale_y_reverse() +
      scale_fill_identity() +
      theme(legend.position="none") +
      theme_void()+
      coord_fixed(ratio=1) +
      labs(title = paste0("Segmentation"))

    # TODO add option to download results: object or background
    return(results_plot)

  })

  output$resultsPlot <- renderPlot({
    if (!input$runButton) {
      # default plot
      #TODO change text
      text = paste("\n   The following is text that'll appear in a plot window.\n",
                   "       As you can see, it's in the plot window\n",
                   "       One might imagine useful information here")
      ggplot() +
        annotate("text", x = 4, y = 25, size=8, label = text) +
        theme_void()
    } else{
      # updated plot
      segmented_image()
    }
    })


  # output$downloadPNG <- downloadHandler(
  #   filename = function() {
  #     str_c('rand_workout_plan_', Sys.Date(), '.png', sep='')
  #   },
  #   contentType = "image/png",
  #   content = function(con) {
  #     gtsave(vals$gg, expand = 50, filename =  con, path = NULL)
  #     # png(con)
  #     # print(vals$gg)
  #     # dev.off()
  #   }
  # )

}

################################################################################
# ui function of the app
################################################################################
# define UI ----
ui <- fluidPage(
  # theme = shinytheme("lumen"),
  theme = light,
  h4("Interactive image segmentation using graphcut variations"),
  titlePanel("Mix Things Up"),
  # Layout a sidebar and main area
  sidebarLayout(
    #side bar here ----
    sidebarPanel(
      width = 3,
      useShinyjs(),
      tags$body(
        h4(strong('Settings: segmentation method')),
        br(),
        p(strong('Available methods:')),
        #TODO create bullets
        p('- single normal distribution'),
        p('- gaussian mixture with fixed number of distributions (to be specified)'),
        p('- gaussian mixture with dynamic number of distributions')
      ),
      selectInput("input_method", "Select method",
                  choices = list("single distribution" = "regular",
                                 "mixture, fixed" = "mixed",
                                 "mixture, dynamic" = "mixed_bic")),

                  # choices = list("normal distribution, single distribution" = "regular",
                  #                "gaussian mixture, fixed number of distributions" = "mixed",
                  #                "gaussian mixture, dynamic number of distributions" = "mixed_bic")),
      selectInput("input_n_dist_object", "Select number of distributions in the object mixture",
                  choices = c(2:10)),
      selectInput("input_n_dist_background", "Select number of distributions in the background mixture",
                  choices = c(2:10))
      ),
    # main area here ----
    mainPanel(
      column(6,
             br(),
             h4(strong('Upload and annotate an image')),
             br(),
             fluidRow(
               #TODO change naming to one convention - camelcase or underscores
               #TODO add error handling - e.g. when no file is selected, but the upload button is clicked
              column(6, fileInput("input_image", "Choose an image", accept = c(".jpg", ".png", ".bmp"))),
              column(6, br(), actionButton("uploadButton", "Upload!", btn_type = "button", class = "btn-secondnary"))),
             plotOutput("uploaded_image", height = 300, width = 300, brush = brushOpts(id = "uploaded_image_brush")),
             actionButton("selectObjectButton", "Mark as part of the object!", btn_type = "button", class = "btn-secondnary"),
             actionButton("selectBackgroundButton", "Mark as part of the background!", btn_type = "button", class = "btn-secondnary"),
             br(),
             actionButton("runButton", "Run classification!", btn_type = "button", class = "btn-secondnary")
             ),
      column(6,
             br(),
             h4(strong('Received segmentation')),
             shinycssloaders::withSpinner(plotOutput("resultsPlot")),
             p('If you are not satisfied with the results, go back to settings and / or change annotations'),
             # align = 'center'
             ),
    )
  )
)

################################################################################
# create a shiny app object
################################################################################
shinyApp(ui = ui, server = server)
