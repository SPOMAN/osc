#' Plot results of graphene curve fit
#'
#' Plots a Raman map, if given a raman curvefit from \code{\link{raman_curvefit_read}} and a column.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @param df A curvefit dataframe from \code{raman_curvefit_read()}
#' @param col_name The name of the column to plot
#' @keywords raman, curve fit
#' @family curve fit functions
#' @export
#' @examples
#' df <- raman_curvefit_read(system.file('extdata/graphene_curve_fit_export', package = 'gRaphene'))
#' plot(df, col_name = `D int`)

plot.raman_curvefit <- function(df, ..., col_name) {
  col_name <- dplyr::enquo(col_name)
  df %>%
    dplyr::mutate(value = !!col_name) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_raster()+
    ggplot2::coord_equal()
}

#' Plot cyclic voltammograms
#'
#' Produces a CV-plot based on a dataframe from \code{\link{cv_read}}.
#' If there is more than one CV in the file, each will be plotted in a different color.
#'
#' @importFrom magrittr %>%
#' @param df A CV loaded with \code{cv_read()}
#' @family cyclic voltammetry
#' @export
#' @examples
#' file <- system.file('extdata/cv/5cv_example.txt', package = 'osc')
#' df <- cv_read(file, skip = 70)
#' plot(df)

plot.cv <- function(df) {
  if(length(unique(df$cv)) > 1) {
    df %>%
      ggplot2::ggplot(ggplot2::aes(x = potential, y = current, color = as.factor(cv))) +
      ggplot2::geom_path() +
      ggplot2::labs(x = "Potential (V)", y = "Current (A)")
  } else {
    df %>%
      ggplot2::ggplot(ggplot2::aes(x = potential, y = current)) +
      ggplot2::geom_path() +
      ggplot2::labs(x = "Potential (V)", y = "Current (A)")

  }
}

#' Plot electrolysis
#'
#' Produces an electrolysis based on a dataframe from \code{\link{electrolysis_read}}.
#'
#' @importFrom magrittr %>%
#' @param df An electrolysis loaded with \code{electrolysis_read()}
#' @family electrolysis, electrochemistry
#' @export
#'

plot.electrolysis <- function(df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = current)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = paste("Electrolysis at ", meta(df, 'E'), " V"), x = "Time (s)", y = "Current (A)")
}

