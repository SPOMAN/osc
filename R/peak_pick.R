#' Pick peaks in datasets
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

peak_picker <- function(df, x, y, find_nearest = TRUE) {
  requireNamespace("shiny", quietly = TRUE)
  requireNamespace("miniUI", quietly = TRUE)

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- generic_df(df, !!x, !!y)


ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select peaks by clicking on the figure below"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot1", height = "100%", click = "plot1_click")
    ),
    miniUI::miniContentPanel(shiny::verbatimTextOutput('list'))
  )

  server <- function(input, output, session) {
    v <- shiny::reactiveValues(
      selectedData = data[0,]
    )

    shiny::observeEvent(input$plot1_click, {
      X1 <- shiny::nearPoints(data, input$plot1_click, maxpoints = 1, threshold = 10)

      if (nrow(X1) > 0 ) {

        if (find_nearest) {
          # Find nearest peak
          peak_ind <- find_peak(data$x, data$y, which(data$x == X1$x))
          X1 <- data[peak_ind,]
        }

        X_bind <- rbind(v$selectedData, X1)

        if (nrow(X_bind) == nrow(dplyr::distinct(X_bind))) {
          v$selectedData <- rbind(v$selectedData, X1)
        } else {
          v$selectedData <- dplyr::anti_join(v$selectedData, X1, by = c("x", "y"))
        }
      }
    })

    output$list<-shiny::renderPrint({
      v$selectedData
    })
    output$plot1 <- shiny::renderPlot({
      ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::geom_line() + ggplot2::geom_point(data = v$selectedData, ggplot2::aes(x, y), color = "red", size = 2)
    })

    shiny::observeEvent(input$done, {
      peaks <- dplyr::left_join(data, dplyr::mutate(v$selectedData, peak = TRUE), by = c("x", "y")) %>% pull(peak)
      peaks <- ifelse(!is.na(peaks), TRUE, FALSE)
      return_data <- df %>% mutate(peak = peaks)


      shiny::stopApp(returnValue = invisible(return_data))
    })
  }
  shiny::runGadget(ui, server)
}


#' Return a standard dataframe with two columns (x and y)
#'
#' @param df
#' @param x
#' @param y
#'
#' @return
#'
generic_df <- function(df, x, y) {
  x = rlang::enquo(x)
  y = rlang::enquo(y)

  tibble::tibble(x = dplyr::pull(df, !!x), y = dplyr::pull(df, !!y))
}


#' Returns the index of the nearest peak
#'
#' @param x
#' @param y
#' @param ind
#' @param prev_max
#'
#' @return
#'
find_peak <- function(x, y, ind) {
  grad <- find_gradient(x, y, ind)
  if (grad > 0) move <- 1 else move <- -1

  if (y[ind + move] > y[ind]) {
    find_peak(x, y, ind + move)
  } else {
    return(ind)
  }
}


#' Local gradient at given index
#'
#' @param x
#' @param y
#' @param ind
#'
#' @return
#'
#' @examples
find_gradient <- function(x, y, ind) {
  (y[ind + 1] - y[ind - 1]) / (x[ind + 1] - x[ind - 1])
}

