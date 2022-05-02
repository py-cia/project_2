library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(tidyverse)
dq_all_shift <- read_csv("C:/Users/valen/OneDrive/Desktop/dq_all_shift.csv")
num_vars <- c("small", "medium", "large")
num_emp <- c("Martha", "jose", "danielle", "honesty", "miguel", "heather")
num_day <- c("morning", "night")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tabsetPanel(
    tabPanel("Employee Cone Weight Distriubtion",
    titlePanel("Dairy Queen Ice Cream Cones!"),
    selectInput("var", "Cone Size(Small: 5oz, Medium: 7oz, Large: 10oz)", choices = num_vars),
    selectInput("name", "Employee", choices = num_emp),
    fluidRow(
      column(6, tableOutput("output")),
      column(6, verbatimTextOutput("sum")
      )
    ),
    plotOutput("hist")
    ),
  tabPanel("Shift vs Shift",
           titlePanel("Which Shift makes the best cones?"),
           fluidRow(
           column(4,
           selectInput("oz", "Cone Size oz", choices = num_vars),
           selectInput("time", "Shift", choices = num_day)
           ),
           column(8,
           verbatimTextOutput("sum2")
           )
           ),
           plotOutput("hist2"),
           titlePanel("How lucky or unlucky are you?"),
           selectInput("size", "What size ice-cream cone would you like? Small: 5oz, Medium: 7oz, Large: 10oz", choices = num_vars),
           verbatimTextOutput("results"),
           )
  )
)
server <- function(input, output, session) {
  x1 <- reactive(rnorm(100, mean = 5))
  x2 <- reactive(rnorm(100, mean = 7))
  x3 <- reactive(rnorm(100, mean = 10))
  output$output <- renderTable(head(dq_all_shift %>% filter(name == input$name, size == input$var)))
  output$sum <- renderPrint(dq_all_shift %>% filter(name == input$name, size == input$var) %>% summary())
  output$hist <- renderPlot({
        ggplot(dq_all_shift %>% filter(name == input$name, size == input$var), aes(weight)) +
          geom_histogram(binwidth = .1)
    })
  output$results <- renderPrint({
    if (input$size == "small") {
      print(sample(x1(), size = 1))
    } else if (input$size == "medium") {
      print(sample(x2(), size = 1))
    } else
      print(sample(x3(), size = 1))
  })
  output$hist2 <- renderPlot({
    ggplot(dq_all_shift %>% filter(size == input$oz), aes(weight, color = shift)) +
      geom_freqpoly()
  })
  output$sum2 <- renderPrint({
    dq_all_shift %>% filter(size == input$oz, shift == input$time) %>% summary()
  })
}

shinyApp(ui, server)
