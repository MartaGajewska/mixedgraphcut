################################################################################
# This is the Shiny App for the main menu of mixedgraphcut
################################################################################


################################################################################
#Set up some variables, define all as global (the <<- notation)
#name of R package
packagename <<- "mixedgraphcut"
#find path to apps
appdir <<- system.file("appinformation", package = packagename) #find path to apps


################################################################################
# define functions
################################################################################

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
server <- function(input, output, session)
{

  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })
}

################################################################################
# ui function of the app
################################################################################
ui <- fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

################################################################################
# create a shiny app object
################################################################################
shinyApp(ui = ui, server = server)
