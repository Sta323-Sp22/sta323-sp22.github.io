## Beta-Binomial

# y ~ Binom(n, p)
#
# y = # heads
# n = # flips
# p = prob. of heads
#
# p ~ Beta(a, b)
#
# a = # of previous successes
# b = # of previous failures
#
# p | y ~ Beta(a + y, b + n - y)
#


## App

library(tidyverse)
library(shiny)

shinyApp(
  ui = fluidPage(
    title = "Beta-Binomial",
    titlePanel("Beta-Binomial Visualizer"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Data:"),
        sliderInput("x", "# of heads", min=0, max=100, value=10),
        sliderInput("n", "# of flips", min=0, max=100, value=20),
        h4("Prior:"),
        numericInput("alpha", "Prior # of head", min=0, value=5),
        numericInput("beta", "Prior # of tails", min=0, value=5),
        h4("Options:"),
        checkboxInput("options", "Show Options", value = FALSE),
        conditionalPanel(
          "input.options == true",
          checkboxInput("bw", "Use theme_bw", value = FALSE),
          checkboxInput("facet", "Use facets", value = FALSE),
        )
        
      ),
      mainPanel = 
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Plot", plotOutput("plot")
            ),
            tabPanel(
              "Table", tableOutput("table")
            )
          )
        )
    )
  ),
  server = function(input, output, session) {
    
    observeEvent(
      input$n,
      {
        updateSliderInput(session, "x", max = input$n)
      }
    )
    
    output$plot = renderPlot({      
      
      d = tibble(
        p = seq(0, 1, length.out = 1000)
      ) %>%
        mutate(
          prior = dbeta(p, input$alpha, input$beta),
          likelihood = dbinom(input$x, size = input$n, prob = p),
          posterior = dbeta(p, input$alpha + input$x, input$beta + input$n - input$x)
        ) %>%
        pivot_longer(
          cols = -p,
          names_to = "distribution",
          values_to = "density"
        ) %>%
        mutate(
          distribution = forcats::as_factor(distribution)
        )
      
      g = ggplot(d, aes(x=p, y=density, color=distribution)) +
        geom_line(size=2)
      
      if (input$bw)
        g = g + theme_bw()
      
      if (input$facet) 
        g = g + facet_wrap(~distribution)
      
      g
    })
  }
)