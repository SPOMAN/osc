# library(tidyverse)
#
# file <- system.file("extdata/cv/cv_example.txt", package = "osc")
# (df <- echem_read(file))
# plot(df)
#
# df <- area_picker(df)
# df <- area(df, sw = 2, x1 = -1.61, x2 = -1.25, p = 3, span = 0.05)
#
# plot_area(df)

#d_new <- area(df, sw = 1, x1 = -2, x2 = -1)

#plot_area(d_new)

# #
# # #--------
# #
# # file <- system.file("extdata/cv/cv_example.txt", package = "osc")
# # df <- echem_read(file)
# #
# #
# # find_peaks <- function(df, ...) {
# #   w <- make_positive(df)
# #
# #   p <- pracma::findpeaks(w, ...)
# #   cat(length(p[,2]), " peaks found!\n", sep = "")
# #   pts <- df$data[p[,2],] %>% mutate(peak = current) %>% select(potential, sweep, peak)
# #   pts
# #   #df$data <- df$data %>% left_join(pts)
# #   #df
# # }
# #
# # make_positive <- function(df) {
# #   df$data %>%
# #     group_by(sweep) %>%
# #     mutate(current = ifelse(direc == "neg", -1 * current, current)) %>%
# #     mutate(current = current - min(current, na.rm = TRUE)) %>%
# #     pull(current)
# # }
# #
# # df$data %>%
# #   mutate(pos = make_positive(df)) %>%
# #   ggplot(aes(potential, pos, color = as.factor(direc))) +
# #   geom_path() +
# #   facet_wrap(~sweep)
# #
# # find_peaks(df, minpeakheight = 1e-7, minpeakdistance = 500) %>% .$data %>%
# #   ggplot(aes(potential,current)) +
# #   geom_path() +
# #   geom_point(aes(potential,peak, group = cv), color = "red") +
# #   facet_wrap(~cv)
# #
# # windowsize <- 0.100
# # pts <- find_peaks(df, minpeakheight = 1e-7, minpeakdistance = 500) %>%
# #   mutate(
# #     min = potential - windowsize,
# #     max = potential + windowsize,
# #     id = 1:n()
# #   )
# #
# # pts
# #
# #
# # #-----------
# # area <- function(df, sw, x1, x2, p = 3, span = 0.01) {
# #   testthat::expect_gt(x1, x2)
# #   testthat::expect_gt(span, 0)
# #   d <- df$data %>%
# #     filter(sweep == sw)
# #
# #   seg1 <- d %>% filter(between(potential, x1 - span, x1 + span))
# #   seg2 <- d %>% filter(between(potential, x2 - span, x2 + span))
# #   bind_rows(seg1, seg2)
# #
# #   bg_data <- bind_rows(seg1, seg2)
# #   bg_fit <- lm(current ~ poly(potential,p), data = bg_data)
# #
# #   d$background <- predict(bg_fit, d)
# #
# #
# #   Q <- d %>%
# #     mutate(subt = d$current - d$background) %>%
# #     filter(between(potential, x2, x1)) %>%
# #     summarise(pracma::trapz(potential, subt)) %>%
# #     as.numeric()
# #
# #   df$area = list(data = d, sweep = sw, x1 = x1, x2 = x2, p = p, span = span, Q = Q)
# #   df
# # }
# #
# #
# # plot_area <- function(df) {
# #   if(is.null(df$area)) stop("Area not calculated. Use area() first.")
# #   df$data %>%
# #     ggplot(aes(potential,current)) +
# #     geom_path() +
# #     #geom_point(aes(potential, peak), color = "red") +
# #     geom_ribbon(data = df$area$data %>% filter(between(potential, df$area$x2, df$area$x1)), aes(ymin = current, ymax = background), fill = "red", alpha = 0.1) +
# #     #geom_point(data = bg_data, aes(x,y), color = "blue") +
# #     geom_line(data = df$area$data %>% filter(between(potential, df$area$x2, df$area$x1)), aes(potential,background), color = "blue")
# # }
# #
# #
# # file <- system.file("extdata/cv/cv_example.txt", package = "osc")
# # df <- echem_read(file)
# # df <- area(df, sw = 1, x1 = -1.4, x2 = -1.85)
# # g1 <- plot_area(df)
# # df$area$Q
# # df <- area(df, sw = 2, x1 = -1.25, x2 = -1.65)
# # g2 <- plot_area(df)
# # df$area$Q
# # gridExtra::grid.arrange(g1, g2, ncol = 1)
# #
# #
# # file <- system.file("extdata/cv/5cv_example.txt", package = "osc")
# # df <- echem_read(file)
# # df <- area(df, sw = 1, x1 = -1.4, x2 = -1.8)
# # g1 <- plot_area(df)
# # df$area$Q
# # df <- area(df, sw = 2, x1 = -1.3, x2 = -1.65)
# # g2 <- plot_area(df)
# # df$area$Q
# # gridExtra::grid.arrange(g1, g2, ncol = 1)
# #
# #
# # #------------------ Peak current
# # d %>% filter(x == pts$x) %>%
# #   mutate(ip = peak-background)
# #
# # #-----------
# #
# # file <- system.file("extdata/cv/5cv_example.txt", package = "osc")
# # df <- echem_read(file)
# # df$data <- df$data[df$data$sweep == 1,]
# # p <- pracma::findpeaks(df$data$current[df$data$sweep == 1])
# # pts <- tibble(x = df$data$potential[p[,2]], peak = df$data$current[p[,2]])
# #
# # window <- tribble(
# #   ~x1, ~x2, ~x3, ~x4,
# #   -1.45, -1.4, -1.8, -1.75
# # )
# #
# #
# # d <- tibble(x = df$data$potential, y = df$data$current) %>%
# #   left_join(pts)
# #
# # seg1 <- d %>% filter(between(x, window$x1, window$x2))
# # seg2 <- d %>% filter(between(x, window$x3, window$x4))
# #
# # bg_data <- bind_rows(seg1, seg2)
# #
# # bg_fit <- lm(y ~ poly(x,3), data = bg_data)
# #
# # d$background <- predict(bg_fit, d)
# #
# # d %>%
# #   ggplot(aes(x,y)) +
# #   geom_path() +
# #   geom_point(aes(x, peak), color = "red") +
# #   geom_ribbon(data = d %>% filter(between(x, (window$x4+window$x3)/2, (window$x1+window$x2)/2)), aes(ymin = y, ymax = background), fill = "red", alpha = 0.1) +
# #   geom_point(data = bg_data, aes(x,y), color = "blue") +
# #   geom_line(data = d %>% filter(between(x, (window$x4+window$x3)/2, (window$x1+window$x2)/2)), aes(x,background), color = "blue")
# #
# # d %>%
# #   mutate(subt = d$y - d$background) %>%
# #   filter(between(x, (window$x4+window$x3)/2, (window$x1+window$x2)/2)) %>%
# #   summarise(pracma::trapz(x, subt))
# #
# # # Peak current
# # d %>% filter(x == pts$x) %>%
# #   mutate(ip = peak-background)
# #
# #
# # pts$x - window$x2
# # pts$x - window$x3
# #
# #
