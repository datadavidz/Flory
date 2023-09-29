#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Flory's most probable distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("mn", "Mn :", 52100, min = 1e4, max = 1e6),
            numericInput("maxr", "Maximum r:", 10000, min = 1e3, max = 1e5),
            sliderInput("steps",
                        "Number of steps:",
                        min = 100,
                        max = 1000,
                        value = 100),
            radioButtons("dist", "Distribution type:",
                         c("Weight density" = "wr",
                           "Number density" = "nc")),
            #                "Log-normal" = "lnorm",
            #                "Exponential" = "exp")),
           actionButton("plot", "Plot")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #Mn <- 52100
  repeat_unit <- 28.05
  #tau <- repeat_unit / Mn

  rdist <- eventReactive(input$plot, {
    tau <- repeat_unit / input$mn
    r <- seq(1, input$maxr, input$steps)
    wr <- tau^2 * r * exp(-tau * r)  
    if (input$dist == "nc") {
      wr <- wr / (seq(1, input$maxr, input$steps) * repeat_unit)
    }
    wr <- wr / sum(wr)
    as_tibble(list(r = r, wr = wr))
  })
  
  output$distPlot <- renderPlot({
    rdist() |>
      ggplot(aes(x = r, y = wr)) +
      geom_point() +
      theme_light()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
