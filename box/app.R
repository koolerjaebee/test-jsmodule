library(shiny)

source("../module/box.R")
source("../module/regress.R")
library(data.table)
library(ggpubr)
library(scales)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      boxUI("box")
    ),
    mainPanel(
      optionUI("box"),
      plotOutput("box_plot"),
      ggplotdownUI("box"),
    )
  )
)


d1 <- mtcars
d1$cyl <- as.factor(d1$cyl)
d1$gear <- as.factor(d1$gear)
d1$vs <- as.factor(d1$vs)


server <- function(input, output, session) {
  data <- reactive(d1)
  data.label <- reactive(jstable::mk.lev(d1))

  out_box <- boxServer("box",
    data = data, data_label = data.label,
    data_varStruct = NULL
  )

  output$box_plot <- renderPlot({
    print(out_box())
  })
}


shinyApp(ui = ui, server = server)