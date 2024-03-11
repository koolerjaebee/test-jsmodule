library(shiny)
# library(ggplot2)
# library(ggpubr)
# 
# library(shinyjs)
# library(logging)
# library(data.table)
source("../module/bar.R")
source("../module/kaplan.R")


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      barUI("bar")
    ),
    mainPanel(
      plotOutput("bar_plot"),
      ggplotdownUI("bar")
    )
  ),
  # useShinyjs(),
)


d1 <- mtcars
d1$cyl <- as.factor(d1$cyl)
d1$gear <- as.factor(d1$gear)
d1$vs <- as.factor(d1$vs)


# basicConfig()
# 
# options(shiny.error = browser)
# 
# options(shiny.error = function() {
#   logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", "))
# })


server <- function(input, output, session) {
  
  # printLogJs <- function(x, ...) {
  #   logjs(x)
  #   T
  # }
  # addHandler(printLogJs)
  
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
