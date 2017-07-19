#' Fit a background and calculate the area between to points
#'
#' @export

area <- function(df, sw, x1, x2, p = 3, span = 0.01) {
  testthat::expect_gt(x1, x2)
  testthat::expect_gt(span, 0)
  d <- df$data %>%
    filter(sweep == sw)

  seg1 <- d %>% filter(between(potential, x1 - span, x1 + span))
  seg2 <- d %>% filter(between(potential, x2 - span, x2 + span))
  bind_rows(seg1, seg2)

  bg_data <- bind_rows(seg1, seg2)
  bg_fit <- lm(current ~ poly(potential,p), data = bg_data)

  d$background <- predict(bg_fit, d)


  Q <- d %>%
    mutate(subt = d$current - d$background) %>%
    filter(between(potential, x2, x1)) %>%
    summarise(pracma::trapz(potential, subt)) %>%
    as.numeric()

  df$area = list(data = d, sweep = sw, x1 = x1, x2 = x2, p = p, span = span, Q = Q)
  df
}

#' Plot fitted area
#'
#' @export
#' @examples
#' file <- system.file("extdata/cv/cv_example.txt", package = "osc")
#' df <- echem_read(file)
#' df <- area(df, sw = 1, x1 = -1.4, x2 = -1.85)
#' g1 <- plot_area(df)
#' df$area$Q
#' df <- area(df, sw = 2, x1 = -1.25, x2 = -1.65)
#' g2 <- plot_area(df)
#' df$area$Q
#' gridExtra::grid.arrange(g1, g2, ncol = 1)

plot_area <- function(df) {
  if(is.null(df$area)) stop("Area not calculated. Use area() first.")
  df$data %>%
    ggplot(aes(potential,current)) +
    geom_path() +
    #geom_point(aes(potential, peak), color = "red") +
    geom_ribbon(data = df$area$data %>% filter(between(potential, df$area$x2, df$area$x1)), aes(ymin = current, ymax = background), fill = "red", alpha = 0.1) +
    #geom_point(data = bg_data, aes(x,y), color = "blue") +
    geom_line(data = df$area$data %>% filter(between(potential, df$area$x2, df$area$x1)), aes(potential,background), color = "blue")
}
