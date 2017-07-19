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
    ggplot2::geom_ribbon(data = df$area$data, ggplot2::aes(ymin = current, ymax = background), alpha = 0.1) +
    #geom_point(data = bg_data, aes(x,y), color = "blue") +
    ggplot2::geom_line(data = df$area$data, ggplot2::aes(potential,background))
  ann <- tibble::tribble(
    ~x, ~y, ~hjust, ~vjust, ~text,
    Inf, Inf, 1, 1, paste(prettyNum(df$area$Q, digits = 3, format = "fg"), "C")
  )
  g + ggplot2::geom_text(data = ann, ggplot2::aes(x, y, label = text, hjust = hjust, vjust = vjust), nudge_x = 0.5, nudge_y = -1)
}
