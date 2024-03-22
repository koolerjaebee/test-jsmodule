library(shiny)

source("~/ShinyApps/Practice/jsmodule_test/module/line.R")
source("~/ShinyApps/Practice/jsmodule_test/module/regress.R")
source("~/ShinyApps/Practice/jsmodule_test/module/kaplan.R")
library(data.table)
library(ggplot2)
library(ggpubr)
library(scales)
library(shinyWidgets)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      lineUI("line")
    ),
    mainPanel(
      optionlineUI("line"),
      plotOutput("line_plot"),
      ggplotdownUI("line")
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

  out_line <- lineServer("line",
    data = data, data_label = data.label,
    data_varStruct = NULL
  )

  output$line_plot <- renderPlot({
    print(out_line())
  })
}


shinyApp(ui = ui, server = server)