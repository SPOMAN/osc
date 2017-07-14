#' Plot results of graphene curve fit
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @keywords raman, curve fit
#' @family curve fit functions
#' @export
#' @examples
#' raman_curvefit_read("data")
#' raman_curvefit_read(system.file('extdata/graphene_curve_fit_export', package = 'gRaphene'))


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
#' @importFrom magrittr %>%
#' @export

plot.cv <- function(df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = pot, y = cur, color = cv)) +
    ggplot2::geom_path() +
    labs(x = "Potential (V)", y = "Current (A)")
}

