# Shiny app notes: 12/3/2019
# from https://shiny.rstudio.com/tutorial/written-tutorial/

# Shiny apps assume there is an app.R file in a folder in your working directory.
# They run by using 
runApp("ForamDB_app")

# within this app.R file, there need to be ui and server functions. 
# The minimum code is:
library(shiny)

# Define UI ----
ui <- fluidPage( # create a display that adjusts to the browser windew
  # multiple options for what goes in here, but often
  titlePanel("title panel"),
  
  sidebarLayout( # always takes two arguments:
    sidebarPanel("sidebar panel", # by default this appears on the left
               img(src = "my_image.png", height = 72, width = 72), # the file must be in a folder named 'www' in the same directory as the app.R script
              # within these you can use html tag functions to e.g. create a paragaph (p), or an image (img)
              # you can also add widgets to this, e.g.
              actionButton("action", label = "Action")
              # All widgets require a name for the widget (character string: to access the widget's value) and a label (character string, but can be ""
    ),
    mainPanel("main panel",
              textOutput("selected_var") # e.g. to take info from a widget and display it here. How that works is defined in the server function
              # output functions require a single arguemnt: a character string that Shiny uses as the name of the reactive element
    )
  ),
  # alternatives to this layout are:
  navbarPage(), # multipage user interface with a navigation bar
  fluidRow() # bulid up the layout from a grid system
)

# Define server logic ----
server <- function(input, output) { 
  # input is a list-like object that stores the current values of all the widgets (saved under the names given to the widgets)
  # builds a list-like object named output that contains all the code needed to update the R objects in the app.
  # the element name should match the name of the reactive element created in ui
  # you don't need to explictly return the output.
  dataInput <- reactive({}) # these only update when that particular widget value changes, so it is more efficient (they have a cache)
  # each input can then be isolated in its own reactive / render function so only the relevant things are re-run each time
  output$selected_var <- renderText({ # render functions take a single argument: an R expression surrounded by {}
    "You have selected this" # this will be re-run every time the widget is used
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)