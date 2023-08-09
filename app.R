#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Flory's most probable distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("steps",
                        "Number of steps:",
                        min = 100,
                        max = 1000,
                        value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  Mn <- 52100
  repeat_unit <- 28.05
  tau <- repeat_unit / Mn
  r <- reactive({seq(0, 10000, input$steps)})
  wr <- reactive({tau^2 * r() * exp(-tau * r())})
  
  output$distPlot <- renderPlot({
    as_tibble(list(r = r(), wr = wr())) |>
      ggplot(aes(x = r, y = wr)) +
      geom_point() +
      theme_light()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
