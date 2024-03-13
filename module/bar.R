#' @title barUI: shiny module UI for barplot
#' @description Shiny module UI for barplot
#' @param id id
#' @param label label
#' @return Shiny module UI for barplot
#' @details Shiny module UI for barplot
#' @examples
#' library(shiny)
#' library(ggplot2)
#' library(ggpubr)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       barUI("bar")
#'     ),
#'     mainPanel(
#'       plotOutput("bar_plot"),
#'       ggplotdownUI("bar")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_bar <- barServer("bar",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$bar_plot <- renderPlot({
#'     print(out_bar())
#'   })
#' }
#' @rdname barUI
#' @export



barUI <- function(id, label = "barplot") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("vars_bar")),
    uiOutput(ns("strata_bar")),
    checkboxInput(ns("fill"), "Fill"),
    checkboxInput(ns("mean"), "Mean_SE"),
    checkboxInput(ns("jitter"), "Jitter"),
    uiOutput(ns("pvalue")),  # new feat. pvalue on plot
    uiOutput(ns("subvar")),
    uiOutput(ns("subval"))
  )
}


# plot option
optionUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  shinyWidgets::dropdownButton(
    uiOutput(ns("option_bar")),
    circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
    tooltip = shinyWidgets::tooltipOptions(title = "Click to see other options !")
  )
}


# Temp
ggplotdownUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    h3("Download options"),
    wellPanel(
      uiOutput(ns("downloadControls")),
      downloadButton(ns("downloadButton"), label = "Download the plot")
    )
  )
}


#' @title barServer: shiny module server for barplot.
#' @description Shiny module server for barplot.
#' @param id id
#' @param data Reactive data
#' @param data_label Reactive data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @return Shiny module server for barplot.
#' @details Shiny module server for barplot.
#' @examples
#' library(shiny)
#' library(ggplot2)
#' library(ggpubr)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       barUI("bar")
#'     ),
#'     mainPanel(
#'       plotOutput("bar_plot"),
#'       ggplotdownUI("bar")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_bar <- barServer("bar",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$bar_plot <- renderPlot({
#'     print(out_bar())
#'   })
#' }
#' @rdname barServer
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom ggpubr ggbarplot
#' @importFrom ggplot2 ggsave
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location



barServer <- function(id, data, data_label, data_varStruct = NULL, nfactor.limit = 10) {
  moduleServer(
    id,
    function(input, output, session) {
      ## To remove NOTE.
      level <- val_label <- variable <- NULL

      if (is.null(data_varStruct)) {
        data_varStruct <- reactive(list(variable = names(data())))
      }


      vlist <- reactive({
        data <- data.table(data(), stringsAsFactors = T)

        factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
        # data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
        factor_list <- mklist(data_varStruct(), factor_vars)

        nclass_factor <- unlist(data[, lapply(.SD, function(x) {
          length(levels(x))
        }), .SDcols = factor_vars])

        group_vars <- factor_vars[nclass_factor >= 2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data)]
        group_list <- mklist(data_varStruct(), group_vars)

        except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data)]

        select_vars <- setdiff(names(data), factor_vars)
        select_list <- mklist(data_varStruct(), select_vars)

        return(list(
          factor_vars = factor_vars, factor_list = factor_list, nclass_factor = nclass_factor, group_vars = group_vars, group_list = group_list, except_vars = except_vars,
          select_vars = select_vars, select_list = select_list
        ))
      })

      output$vars_bar <- renderUI({
        tagList(
          selectizeInput(session$ns("x_bar"), "X variable",
            choices = vlist()$factor_vars, multiple = F,
            selected = vlist()$select_vars[1]
          ),
          selectizeInput(session$ns("y_bar"), "Y variable",
            choices = vlist()$select_list, multiple = F,
            selected = ifelse(length(vlist()$select_vars) > 1, vlist()$select_vars[2], vlist()$select_vars[1])
          )
        )
      })

      output$strata_bar <- renderUI({
        strata_vars <- setdiff(vlist()$factor_vars, vlist()$except_vars)
        strata_vars <- setdiff(strata_vars, input$x_bar)
        strata_list <- mklist(data_varStruct(), strata_vars)
        strata_select <- c("None", strata_list)
        selectizeInput(session$ns("strata"), "Strata",
          choices = strata_select, multiple = F,
          selected = unlist(strata_select)[1]
        )
      })
      
      
      # ## pvalue output UI
      # output$pvalue <- renderUI({
      #   req(!is.null(input$x_bar))
      #   
      #   nclass.xbar <- input$x_bar
      #   nclass.factor <- vlist()$nclass_factor[nclass.xbar]
      #   
      #   value.isPvalue <- isolate(input$isPvalue)
      #   value.isPair <- isolate(input$isPair)
      #   
      #   if (nclass.factor < 3) {
      #     # Check sample number and set choices
      #     tagList(
      #       checkboxInput(session$ns("isPvalue"), "P value?", value = value.isPvalue),
      #       radioButtons(
      #         session$ns("pvalue"),
      #         "P value test",
      #         inline = TRUE,
      #         choices = c("T-test"="t.test", "Wilcoxon"="wilcox.test"),
      #       )
      #     )
      #   } else {
      #     tagList(
      #       checkboxInput(session$ns("isPvalue"), "P value?", value = value.isPvalue),
      #       radioButtons(
      #         session$ns("pvalue"),
      #         "P value test",
      #         inline = TRUE,
      #         choices = c("ANOVA"="anova", "Kruskal-Wallis"="kruskal.test")
      #       ),
      #       # Check sample number and set visibility
      #       checkboxInput(session$ns("isPair"), "Pair sample P value?", value = value.isPair),
      #       
      #       # Check sample number and set visibility
      #       radioButtons(
      #         session$ns("p_pvalue"),
      #         "Pair sample P value test",
      #         inline = TRUE,
      #         choices = c("T-test"="t.test", "Wilcoxon"="wilcox.test")
      #       )
      #     )
      #   }
      # })
      
      # Refactoring render UI depends on nclass factor numbers
      output$pvalue <- renderUI({
        req(!is.null(input$x_bar))
        
        if (vlist()$nclass_factor[input$x_bar] < 3) {
          tagList(
            checkboxInput(session$ns("isPvalue"), "P value?"),
            radioButtons(
              session$ns("pvalue"),
              "P value test",
              inline = TRUE,
              choices = c("T-test"="t.test", "Wilcoxon"="wilcox.test")
            ),
            tabsetPanel(
              id = session$ns("side_tabset_pval"),
              type = "hidden",
              selected = "under_three",
              tabPanel(
                "under_three",
              ),
              tabPanel(
                "over_three",
                checkboxInput(session$ns("isPair"), "Pair sample P value?"),
                radioButtons(
                  session$ns("p_pvalue"),
                  "Pair sample P value test",
                  inline = TRUE,
                  choices = c("T-test"="t.test", "Wilcoxon"="wilcox.test")
                  )
                )
              )
            )
        } else {
          tagList(
            checkboxInput(session$ns("isPvalue"), "P value?"),
            radioButtons(
              session$ns("pvalue"),
              "P value test",
              inline = TRUE,
              choices = c("ANOVA"="anova", "Kruskal-Wallis"="kruskal.test")
            ),
            tabsetPanel(
              id = session$ns("side_tabset_pval"),
              type = "hidden",
              selected = "over_three",
              tabPanel(
                "under_three",
              ),
              tabPanel(
                "over_three",
                checkboxInput(session$ns("isPair"), "Pair sample P value?"),
                radioButtons(
                  session$ns("p_pvalue"),
                  "Pair sample P value test",
                  inline = TRUE,
                  choices = c("T-test"="t.test", "Wilcoxon"="wilcox.test")
                )
              )
            )
          )
        }
      })
      
      
      # # Debugging
      # observeEvent(input$x_bar, {
      #   message(paste0("input$x_bar :", input$x_bar))
      #   message(paste0("nclass_factor: ", vlist()$nclass_factor[input$x_bar]))
      # })
      

      observeEvent(input$subcheck, {
        output$subvar <- renderUI({
          req(input$subcheck == T)
          req(!is.null(input$x_bar))

          var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$x_bar, input$y_bar, input$strata))

          var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
          validate(
            need(length(var_subgroup) > 0, "No variables for sub-group analysis")
          )

          tagList(
            selectInput(session$ns("subvar_km"), "Sub-group variables",
              choices = var_subgroup_list, multiple = T,
              selected = var_subgroup[1]
            )
          )
        })
      })
      
      # Observe xbar
      observeEvent(input$x_bar, {
        nclass.factor <- vlist()$nclass_factor[input$x_bar]
        
        if (nclass.factor < 3) {
          updateTabsetPanel(session, "side_tabset_pval", selected = "under_three")
          updateTabsetPanel(session, "dropdown_tabset_pval", selected = "under_three")
        } else {
          updateTabsetPanel(session, "side_tabset_pval", selected = "over_three")
          updateTabsetPanel(session, "dropdown_tabset_pval", selected = "over_three")
        }
      })
      
      
      # Reset button observe
      observeEvent(input$pval_reset, {
        updateSliderInput(session, "pvalfont", value = 4)
        updateSliderInput(session, "pvalx", value = 0.5)
        updateSliderInput(session, "pvaly", value = 1)
        updateSliderInput(session, "p_pvalfont", value = 4)
      })


      output$subval <- renderUI({
        req(input$subcheck == T)
        req(length(input$subvar_km) > 0)

        outUI <- tagList()

        for (v in seq_along(input$subvar_km)) {
          if (input$subvar_km[[v]] %in% vlist()$factor_vars) {
            outUI[[v]] <- selectInput(session$ns(paste0("subval_km", v)), paste0("Sub-group value: ", input$subvar_km[[v]]),
              choices = data_label()[variable == input$subvar_km[[v]], level], multiple = T,
              selected = data_label()[variable == input$subvar_km[[v]], level][1]
            )
          } else {
            val <- stats::quantile(data()[[input$subvar_km[[v]]]], na.rm = T)
            outUI[[v]] <- sliderInput(session$ns(paste0("subval_km", v)), paste0("Sub-group range: ", input$subvar_km[[v]]),
              min = val[1], max = val[5],
              value = c(val[2], val[4])
            )
          }
        }
        outUI
      })
      
      
      barInput <- reactive({
        req(c(input$x_bar, input$y_bar, input$strata, input$pvalue, input$pvalx, input$pvaly, input$pvalfont, input$p_pvalue, input$p_pvalfont))
        req(input$isPair != "None")
        req(input$isPvalue != "None")
        
        data <- data.table(data())
        label <- data_label()
        color <- ifelse(input$strata == "None", "black", input$strata)
        fill <- "white"
        if (input$fill) {
          fill <- ifelse(input$strata == "None", input$x_bar, input$strata)
        }

        if (input$strata != "None") {
          data <- data[!is.na(get(input$strata))]
        }
        add.params <- list()
        cor.coeff.args <- list(p.accuracy = 0.001)

        add <- "mean"
        if (input$jitter) {
          add <- c("mean", "jitter")
        }
        if (input$mean) {
          add <- "mean_se"
        }
        if (input$mean & input$jitter) {
          add <- c("jitter", "mean_se")
        }
        
        pval.name <- input$pvalue
        ppval.name <- input$p_pvalue
        
        if (is.null(input$pvalfont)) {
          pval.font.size <-  c(4, 4, 0.4) %>% isolate
          pval.coord <-  c(0.5, 1) %>% isolate
        } else {
          pval.font.size = c(input$pvalfont, input$p_pvalfont, input$p_pvalfont / 10) %>% isolate
          pval.coord = c(input$pvalx, input$pvaly) %>% isolate
        }

        
        ggpubr::ggbarplot(data, input$x_bar, input$y_bar,
          color = color, add = add, add.params = add.params, conf.int = input$lineci,
          xlab = label[variable == input$x_bar, var_label][1],
          ylab = label[variable == input$y_bar, var_label][1], na.rm = T,
          position = position_dodge(), fill = fill,
        ) +
          {
            if (input$isPvalue) {
              stat_compare_means(
                method = pval.name,
                size = pval.font.size[1],
                label.x.npc = pval.coord[1],
                label.y.npc = pval.coord[2],
                aes(
                  label = scales::label_pvalue(add_p = TRUE)(after_stat(p))
                ),
              )
            }
          } +
              {
                if (input$isPair && vlist()$nclass_factor[input$x_bar] > 2) {
                  geom_pwc(
                    method = ppval.name,
                    size = pval.font.size[3],
                    label.size = pval.font.size[2],
                    aes(
                      label = scales::label_pvalue(add_p = TRUE)(after_stat(p))
                      ),
                  )
                  }}
        })

      output$downloadControls <- renderUI({
        tagList(
          column(
            4,
            selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
              choices = c("jpg", "pdf", "tiff", "svg", "pptx"), multiple = F,
              selected = "pptx"
            )
          ),
          column(
            4,
            sliderInput(session$ns("fig_width"), "Width (in):",
              min = 5, max = 15, value = 8
            )
          ),
          column(
            4,
            sliderInput(session$ns("fig_height"), "Height (in):",
              min = 5, max = 15, value = 6
            )
          )
        )
      })

      output$downloadButton <- downloadHandler(
        filename = function() {
          paste(input$x_bar, "_", input$y_bar, "_barplot.", input$file_ext, sep = "")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          withProgress(
            message = "Download in progress",
            detail = "This may take a while...",
            value = 0,
            {
              for (i in 1:15) {
                incProgress(1 / 15)
                Sys.sleep(0.01)
              }

              if (input$file_ext == "pptx") {
                my_vec_graph <- rvg::dml(ggobj = barInput())

                doc <- officer::read_pptx()
                doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
                print(doc, target = file)
              } else {
                ggplot2::ggsave(file, barInput(), dpi = 300, units = "in", width = input$fig_width, height = input$fig_height)
              }
            }
          )
        }
      )
      
      
      # option dropdown menu
      output$option_bar <- renderUI({
        
        if (vlist()$nclass_factor[input$x_bar] < 3) {
          tabset.selected <- "under_three"
        } else {
          tabset.selected <- "over_three"
        }
        
        tagList(
          h3("P-value position"),
          sliderInput(session$ns("pvalfont"), "P-value font size",
                      min = 1, max = 10, value = 4),
          sliderInput(session$ns("pvalx"), "x-axis",
                      min = 0, max = 1, value = 0.5
          ),
          sliderInput(session$ns("pvaly"), "y-axis",
                      min = 0, max = 1, value = 1
          ),
          tabsetPanel(
            id = session$ns("dropdown_tabset_pval"),
            type = "hidden",
            selected = tabset.selected,
            tabPanel(
              "under_three",
            ),
            tabPanel(
              "over_three",
              h3("Pair P-value position"),
              sliderInput(session$ns("p_pvalfont"), "P-value font size",
                          min = 1, max = 10, value = 4),
            )
          ),
          actionButton(session$ns("pval_reset"), "reset")
        )
        
        # if (input$isPair) {
        #   tagList(
        #     h3("P-value position"),
        #     sliderInput(session$ns("pvalfont"), "P-value font size",
        #                 min = 1, max = 10, value = 4),
        #     sliderInput(session$ns("pvalx"), "x-axis",
        #                 min = 0, max = 1, value = 0.5
        #     ),
        #     sliderInput(session$ns("pvaly"), "y-axis",
        #                 min = 0, max = 1, value = 1
        #     ),
        #     tabsetPanel(
        #       id = session$ns("dropdown_tabset_pval"),
        #       type = "hidden",
        #       tabPanel(
        #         "under three",
        #       ),
        #       tabPanel(
        #         "over three",
        #         h3("Pair P-value position"),
        #         sliderInput(session$ns("p_pvalfont"), "P-value font size",
        #                     min = 1, max = 10, value = 4),
        #       )
        #     ),
        #     actionButton(session$ns("pval_reset"), "reset")
        #   )
        # } else {
        #   tagList(
        #     h3("P-value position"),
        #     sliderInput(session$ns("pvalfont"), "P-value font size",
        #                 min = 1, max = 10, value = 4),
        #     sliderInput(session$ns("pvalx"), "x-axis",
        #                 min = 0, max = 1, value = 0.5
        #     ),
        #     sliderInput(session$ns("pvaly"), "y-axis",
        #                 min = 0, max = 1, value = 1
        #     ),
        #     tabsetPanel(
        #       id = session$ns("dropdown_tabset_pval"),
        #       type = "hidden",
        #       tabPanel(
        #         "under three",
        #       ),
        #       tabPanel(
        #         "over three",
        #         h3("Pair P-value position"),
        #         sliderInput(session$ns("p_pvalfont"), "P-value font size",
        #                     min = 1, max = 10, value = 4),
        #       )
        #     ),
        #     actionButton(session$ns("pval_reset"), "reset")
        #   )
        # }
      })

      return(barInput)
    }
  )
}




#####


#
#
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
#   mtcars$am <- as.factor(mtcars$am)
#   mtcars$vs <- as.factor(mtcars$vs)
#   mtcars$gear <- as.factor(mtcars$gear)
#   mtcars$carb <- as.factor(mtcars$carb)
#   mtcars$cyl <- as.factor(mtcars$cyl)
#   data <- reactive(mtcars)
#   data.label <- reactive(jstable::mk.lev(mtcars))
#   out_bar <- barServer("bar", data = data, data_label = data.label,
#                            data_varStruct = NULL)
#
#   output$bar_plot <- renderPlot({
#     print(out_bar())
#   })
# }
#
# shinyApp(ui, server)
