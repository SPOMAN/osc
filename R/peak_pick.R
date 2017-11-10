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
  input_name <- deparse(substitute(df))

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- generic_df(df, !!x, !!y)


ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select peaks by clicking on the figure below"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot1", height = "100%", click = "plot1_click")
    )#,
    #miniUI::miniContentPanel(shiny::verbatimTextOutput('list'))
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
          v$selectedData <- rbind(v$selectedData, X1) %>% dplyr::arrange(x)
        } else {
          v$selectedData <- dplyr::anti_join(v$selectedData, X1, by = c("x", "y"))
        }
      }
    })

    output$list<-shiny::renderPrint({
      v$selectedData
    })
    output$plot1 <- shiny::renderPlot({
      peak_indices <- match(v$selectedData$x, data$x)
      data %>% add_peaks(peak_indices) %>% plot_peaks(x, y)
    })

    shiny::observeEvent(input$done, {
      peak_indices <- match(v$selectedData$x, data$x)
      return_data <- df %>% add_peaks(peak_indices)

      cat(paste0(length(peak_indices)," found in the dataset\n"))
      cat("The add_peaks() function below has been copied to the clipboard!\n")
      cat("Please paste it in your script for reproducibility.\n")
      peak_vec <- paste(peak_indices, collapse = ",")
      res_string = paste0(input_name, ' <- ', input_name,' %>% add_peaks(c(', peak_vec ,'))')
      cat(paste0("    ", res_string, "\n"))
      clipr::write_clip(res_string, return_new = FALSE)

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


#' Add peak indicators at the given indices
#'
#' @param df
#' @param indices
#'
#' @return
#' @export
#'
#' @examples
add_peaks <- function(df, indices) {
  return_data <- df %>% mutate(peak = FALSE)
  return_data$peak[indices] <- TRUE
  return_data
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

#' Title
#'
#' @param df
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
plot_peaks <- function(df, x, y) {
  if (!("peak" %in% colnames(df))) stop("Column peaks not found")

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- generic_df(df, !!x, !!y) %>% mutate(peak = df$peak)

  nudge_dist <- (max(data$x) - min(data$x)) / 100
  data %>%
    ggplot(aes(x, y)) +
    geom_line() +
    geom_text(data = . %>% filter(peak), aes(x, y, label = round(x)), size = 3, nudge_y = nudge_dist * 9, color = "red") +
    geom_segment(data = . %>% filter(peak), aes(x = x, xend = x, y = y + nudge_dist*2, yend = y + nudge_dist*5), color = "red")
}
