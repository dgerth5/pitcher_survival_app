# ui
library(shiny)

ui <- navbarPage(
  title = "How Long Will a Pitcher Pitch After His Rookie Year?",
  
  tabPanel(
    title = "Comparison",
    fluidPage(
      titlePanel("Time Until Retirement Comparison"),
      
      fluidRow(
        column(1, div(style = "margin-top: 25px;", strong("Pitcher 1:"))),
        column(2, numericInput("Age_Row1", "Age", value = 25)),
        column(2, numericInput("IP_Row1", "IP", value = 100)),
        column(2, numericInput("k_Row1", "K%+", value = 100)),
        column(2, numericInput("b_Row1", "BB%+", value = 100)),
        column(2, selectInput("starter_Row1", "Starter?", choices = c("Yes" = 1, "No" = 0), selected = 1))
      ),
      
      fluidRow(
        column(1, div(style = "margin-top: 25px;", strong("Pitcher 2:"))),
        column(2, numericInput("Age_Row2", "Age", value = 25)),
        column(2, numericInput("IP_Row2", "IP", value = 100)),
        column(2, numericInput("k_Row2", "K%+", value = 100)),
        column(2, numericInput("b_Row2", "BB%+", value = 100)),
        column(2, selectInput("starter_Row2", "Starter?", choices = c("Yes" = 1, "No" = 0), selected = 0))
      ),
      
      plotOutput("survivalPlot")
    )
  ),
  
  tabPanel(
    title = "Explanation",
    fluidPage(
      titlePanel("Explanation"),
      p("This is a Weibull survival model, which takes in a pitcher's rookie season innings pitched, K%+, BB%+ and if he was a starter, and outputs survival curves to give an idea of how long a pitcher's career will last after his rookie season. The model was built using Stan.")
    )
  )
)
