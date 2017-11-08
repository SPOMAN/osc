#' Pick peaks in datasets
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

peak_picker <- function(df) {
  requireNamespace("shiny", quietly = TRUE)
  requireNamespace("miniUI", quietly = TRUE)


ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select peaks by clicking on the figure below"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot1", height = "100%", click = "plot1_click")
    ),
    miniUI::miniContentPanel(shiny::verbatimTextOutput('list'))
  )

  server <- function(input, output, session) {
    v <- shiny::reactiveValues(
      selectedData = df[0,]
    )

    shiny::observeEvent(input$plot1_click, {
      X1 <- shiny::nearPoints(df, input$plot1_click, maxpoints = 1)
      if (is.null(v$selectedData)) {
        v$selectedData <- X1
      } else {
        if (nrow(merge(X1, v$selectedData)) > 0) {
          ind <- shiny::anyDuplicated(rbind(v$selectedData, X1), fromLast=TRUE)
          v$selectedData <- v$selectedData[-ind,]
        } else {
          v$selectedData <- rbind(v$selectedData, X1)
        }
      }
    })

    output$list<-shiny::renderPrint({
      v$selectedData
    })
    output$plot1 <- shiny::renderPlot({
      ggplot2::ggplot(df, ggplot2::aes(wavenumber, int_mean)) + ggplot2::geom_line() + ggplot2::geom_point(data = v$selectedData, ggplot2::aes(wavenumber, int_mean), color = "red", size = 2)
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(returnValue = invisible(v$selectedData))
    })
  }
  shiny::runGadget(ui, server)
}

