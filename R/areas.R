#' Fit a background and calculate the area between to points
#'
#' @param df Data from [echem_read()]
#' @param sw The sweep number to be used
#' @param x1 Limit of integration (lower). Must be smaller than x2.
#' @param x2 Limit of integration (upper). Must be larger than x1.
#' @param p Degree of polynomial used as background
#' @param span Width of the window used for fitting at either end of the
#'   background polynomial. Defaults to 50 mV.
#' @family area functions
#' @export

area <- function(df, sw, x1, x2, p = 3, span = 0.05) {
  if (x1 > x2) stop("x1 must be smaller than x2")
  if (span < 0) stop("span cannot be negative")
  span <- span / 2
  d <- df$data %>%
    dplyr::filter(sweep == sw, dplyr::between(potential, x1 - span, x2 + span))

  seg1 <- d %>% dplyr::filter(dplyr::between(potential, x1 - span, x1 + span))
  seg2 <- d %>% dplyr::filter(dplyr::between(potential, x2 - span, x2 + span))
  dplyr::bind_rows(seg1, seg2)

  bg_data <- dplyr::bind_rows(seg1, seg2)
  bg_fit <- lm(current ~ poly(potential,p), data = bg_data)

  d$background <- predict(bg_fit, d)


  Q <- d %>%
    dplyr::mutate(subt = d$current - d$background) %>%
    dplyr::summarise(pracma::trapz(potential, subt)) %>%
    as.numeric()

  df$area = list(data = d, sweep = sw, x1 = x1, x2 = x2, p = p, span = span, Q = Q)
  df
}

#' Plot fitted area
#'
#' @family area functions
#' @export
#' @examples
#' file <- system.file("extdata/cv/cv_example.txt", package = "osc")
#' df <- echem_read(file)
#'
#' df1 <- area(df, sw = 1, x1 = -1.85, x2 = -1.40)
#' g1 <- plot_area(df1)
#' df2 <- area(df, sw = 2, x1 = -1.65, x2 = -1.25)
#' g2 <- plot_area(df2)
#'
#' gridExtra::grid.arrange(g1, g2, ncol = 1)

plot_area <- function(df) {
  if(is.null(df$area)) stop("Area not calculated. Use area() first.")
  g <- df$data %>%
    ggplot2::ggplot(ggplot2::aes(potential,current)) +
    ggplot2::geom_path() +
    #geom_point(aes(potential, peak), color = "red") +
    ggplot2::geom_ribbon(data = df$area$data, ggplot2::aes(ymin = current, ymax = background, fill = (current - background < 0)), alpha = 0.5) +
    #geom_point(data = bg_data, aes(x,y), color = "blue") +
    ggplot2::geom_line(data = df$area$data, ggplot2::aes(potential,background), color = "blue") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_line(data = df$area$data %>% dplyr::filter(dplyr::between(potential, df$area$x1 - df$area$span, df$area$x1 + df$area$span)), ggplot2::aes(potential, current), color = "red") +
    ggplot2::geom_line(data = df$area$data %>% dplyr::filter(dplyr::between(potential, df$area$x2 - df$area$span, df$area$x2 + df$area$span)), ggplot2::aes(potential, current), color = "red")
  ann <- tibble::tribble(
    ~x, ~y, ~hjust, ~vjust, ~text,
    Inf, Inf, 1, 1, paste(prettyNum(df$area$Q, digits = 3, format = "fg"), "C")
  )
  g + ggplot2::geom_text(data = ann, ggplot2::aes(x, y, label = text, hjust = hjust, vjust = vjust), nudge_x = 0.5, nudge_y = -1)
}



#' area_picker
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' df <- echem_read(system.file('extdata/cv/cv_example.txt', package = 'osc'))
#' df <- area_picker(df)

area_picker <- function(df) {
  requireNamespace("shiny", quietly = TRUE)
  requireNamespace("miniUI", quietly = TRUE)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select integration limits by dragging sliders below"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot", height = "100%")
    ),
    miniUI::miniContentPanel(
      shiny::fillRow(
        shiny::sliderInput("x1", "Integration limits", min(df$data$potential), max(df$data$potential), c(min(df$data$potential), max(df$data$potential)), sep = "", post = "V", dragRange = TRUE, width = "100%", step = 0.01),
        height = "50%"
      ),
      shiny::fillRow(
        shiny::selectInput("sweep", "Sweep", unique(df$data$sweep), width = "90%"),
        shiny::sliderInput("poly", "Background polynomial degree", 1, 5, 3, width = "90%", step = 1),
        shiny::sliderInput("span", "Span of fitting endpoints", 0.01, 0.25, 0.05, post = "V", width = "90%"),
        height = "50%")
    )
  )
  server <- function(input, output, session) {
    output$plot <- shiny::renderPlot({
      a <- area(df, sw = input$sweep, x1 = input$x1[1], x2 = input$x1[2], p = input$poly, span = input$span)
      plot_area(a)
    })
    shiny::observeEvent(input$done, {
      a <- area(df, sw = input$sweep, x1 = input$x1[1], x2 = input$x1[2], p = input$poly, span = input$span)
      cat(paste0("\nArea of ", prettyNum(a$area$Q, digits = 3, format = "fg"), " C found between ", a$area$x1 , " V and ", a$area$x2 , " V.\n"))
      cat("Please paste the following into your script for reproducibility:\n")
      cat(paste0("    df <- area(df, sw = ", input$sweep, ", x1 = ", input$x1[1], ", x2 = ", input$x1[2], ", p = ", input$poly, ", span = ", input$span, ")\n"))
      shiny::stopApp(returnValue = )
    })
  }
  shiny::runGadget(ui, server)
}
