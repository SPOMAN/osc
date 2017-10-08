#' Load results of graphene curve fit
#'
#' Attempts to load all curve fits in txt files from the specified directory
#'
#' All files should be named as "<peak> <feature>.txt", e.g. "2D FWHM.txt" and be put in the folder specified under \code{path}.
#' The dataframe that is returned will contain the "<peak> <feature>" as the column names - for this to work, the filenames should only contain letters, numbers and spaces.
#'
#' @importFrom magrittr %>%
#' @param path Directory containing the data-files
#' @param ext Extension of data-files (default is 'txt')
#' @keywords raman, curve fit
#' @family curve fit functions
#' @export
#' @examples
#' raman_curvefit_read("data")
#' raman_curvefit_read(system.file('extdata/graphene_curve_fit_export', package = 'osc'))


raman_curvefit_read <- function(path, ext = "txt") {
  filenames <- list.files(pattern = stringr::str_c('*.',ext), path = path)

  data <- tibble::tibble(filename = filenames) %>%
    dplyr::mutate(path = stringr::str_c(path, "/", filename),
      data = purrr::map(path, readr::read_tsv, col_names = c("x", "y", "value"), skip = 1),
      measure = stringr::str_extract(filenames, pattern = "[0-9a-zA-ZæøåÆØÅ\\s]*")) %>%
    tidyr::unnest() %>%
    dplyr::mutate(xy = stringr::str_c(x, ", ", y)) %>%
    dplyr::select(xy, x, y, measure, value) %>%
    tidyr::spread(key = measure, value = value) %>%
    dplyr::mutate(id = 1:n()) %>%
    dplyr::select(id, dplyr::everything(), -xy)

  if (all(c('D int', 'G int') %in% colnames(data))) {
    data <- data %>% dplyr::mutate(`D/G-ratio` = `D int` / `G int`)
  }
  if (all(c('2D int', 'G int') %in% colnames(data))) {
    data <- data %>% dplyr::mutate(`2D/G-ratio` = `2D int` / `G int`)
  }
  if (all(c('Dp int', 'D int') %in% colnames(data))) {
    data <- data %>% dplyr::mutate(`D/Dp-ratio` = `D int` / `Dp int`)
  }
  class(data) <- c("raman_curvefit", class(data))
  data
}


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
