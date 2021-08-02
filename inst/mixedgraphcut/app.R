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
library(ggplot2)
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

  vals <- reactiveValues()

  # define set_or_sets variable
  set_or_sets <- reactive({
    if(input$n_sets > 1){"sets"}
    else {"set"}
  })

  observe({
    hide("selectionObject")
    if(input$uploadButton)
      show("selectionObject")
  })

  observe({
    hide("selectionBackground")
    if(input$uploadButton)
      show("selectionBackground")
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
        theme_void()
  })

  output$uploaded_image <- renderPlot({plot_image_df(input_image_df())})

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
      useShinyjs(),

      #define input sliders ----
      fileInput("input_image", "Step 1: Upload an image", accept = c(".jpg", ".png", ".bmp")),
      #action button to run setup ----
      actionButton("uploadButton", "Upload!", btn_type = "button", class = "btn-secondnary"),
    ),
    # main area here ----
    mainPanel(
      br(),
      column(12, plotOutput("uploaded_image", height = 300, width = 300,
                 brush = brushOpts(id = "uploaded_image_brush")),
             align='center'),
      br(),
      column(12,
             actionButton("selectionObject", "Mark as part of the object!", btn_type = "button", class = "btn-secondnary"),
             actionButton("selectionBackground", "Mark as part of the background!", btn_type = "button", class = "btn-secondnary"),
             align='center')
    )
  )
)

################################################################################
# create a shiny app object
################################################################################
shinyApp(ui = ui, server = server)
