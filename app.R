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
          numericInput("repunit", "Repeat Unit:", 28, min = 10, max = 2000),
          numericInput("mn", "Mn :", 30000, min = 1e4, max = 1e6),
          numericInput("maxr", "Maximum r:", 10000, min = 1e3, max = 1e5),
          sliderInput("steps",
                        "Number of steps:",
                        min = 100,
                        max = 1000,
                        value = 100),
          actionButton("plot", "Plot")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          radioButtons("dist", "Distribution type:",
                       c("Weight density" = "wr",
                         "Number density" = "nc")), 
          plotOutput("distPlot"),
          tableOutput("mwavgTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #Mn <- 52100
  #repeat_unit <- 28.05
  #tau <- repeat_unit / Mn

  rdist <- eventReactive(input$plot, {
    tau <- input$repunit / input$mn
    r <- seq(1, input$maxr, input$steps)
    wr <- tau^2 * r * exp(-tau * r)
    wr <- wr / sum(wr)
    #if (input$dist == "nc") {
    nc <- wr / (seq(1, input$maxr, input$steps) * input$repunit)
    nc <- nc / sum(nc)
    #}
    #wr <- wr / sum(wr)
    
    as_tibble(list(r = r, wr = wr, nc = nc))
  })
  
  output$distPlot <- renderPlot({
    if (input$dist == "wr") {
      rdist() |>
        ggplot(aes(x = r, y = wr)) +
        geom_point() +
        theme_light()
    } else {
      rdist() |>
        ggplot(aes(x = r, y = nc)) +
        geom_point() +
        theme_light()
    }
  })
  
  output$mwavgTable <- renderTable({
    m_z <- mean(rdist()$wr)
    sd_z <- sd(rdist()$wr)
    
    results <- data.frame(0, 0)
    results[1,1] = format(m_z, digits = 3)
    results[1,2] = format(sd_z, digits = 4)
    colnames(results) <- c("Mean z", "SD z")
    rownames(results) <- c("Stats")
    results
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
