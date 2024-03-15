library(shiny)
options(shiny.sanitize.errors = F)
source("~/ShinyApps/Practice/jsmodule_test/module/bar.R")
source("~/ShinyApps/Practice/jsmodule_test/module/regress.R")
library(data.table)
library(ggplot2)
library(ggpubr)
library(scales)
library(shinyWidgets)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      barUI("bar")
    ),
    mainPanel(
      optionUI("bar"),
      plotOutput("bar_plot"),
      ggplotdownUI("bar")
    )
  ),
)


# d1 <- mtcars
# d1$cyl <- as.factor(d1$cyl)
# d1$gear <- as.factor(d1$gear)
# d1$vs <- as.factor(d1$vs)


d1 <- read.csv("~/ShinyApps/Practice/jsmodule_test/data/example_basic.csv", header = T)

vars.fac <- c()
for (x in 1:ncol(d1)) {
  if(d1[,x] %>% unique %>% unlist %>% length < 6) {
    vars.fac <- append(vars.fac, x)
  }
}

for (x in vars.fac) {
  d1[,x] <- as.factor(d1[,x])
}

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


shinyApp(ui = ui, server = server)
