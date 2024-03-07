library(shiny)
library(ggplot2)
library(ggpubr)
library(jsmodule)

library(data.table)
source("../module/bar.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      barUI("bar")
    ),
    mainPanel(
      plotOutput("bar_plot"),
      ggplotdownUI("bar")
    )
  )
)

d1 <- mtcars
d1


d1$cyl <- as.factor(d1$cyl)
d1$gear <- as.factor(d1$gear)


server <- function(input, output, session) {
  data <- reactive(d1)
  data.label <- reactive(jstable::mk.lev(d1))

  out_bar <- barServer("bar",
    data = data, data_label = data.label,
    data_varStruct = NULL
  )

  output$bar_plot <- renderPlot({
    print(out_bar())
  })
}



# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       barUI("bar")
#     ),
#     mainPanel(
#       plotOutput("bar_plot"),
#       ggplotdownUI("bar")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   data <- reactive(mtcars)
#   data.label <- reactive(jstable::mk.lev(mtcars))
# 
#   out_bar <- barServer("bar",
#     data = data, data_label = data.label,
#     data_varStruct = NULL
#   )
# 
#   output$bar_plot <- renderPlot({
#     print(out_bar())
#   })
# }


shinyApp(ui = ui, server = server)
