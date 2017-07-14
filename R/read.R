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
#' raman_curvefit_read(system.file('extdata/graphene_curve_fit_export', package = 'gRaphene'))


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


#' Load cyclic voltammogram
#'
#' Reads *.txt files from the CHI potentiostats. Automatically detects sweep- and cv-number, so these can later be used for colors in plots etc.
#'
#' @importFrom magrittr %>%
#' @param path Path to the cv file (txt)
#' @param skip Number of lines to skip (where does metadata end and the data start?)
#' @keywords cyclic voltammetry, electrochemistry
#' @export
#' @examples
#' file1 <- "C:/Users/emilbp/PhD/Projects/High-Shear Exfoliation/Data/20150602 CV of benzophenone/CV2 2mM benzophenone.txt"
#' df <- cv_read(file1, skip = 41)


cv_read <- function(file, skip, col_names = c("pot", "cur"), ...) {
  data <- readr::read_csv(file, skip = skip, col_names = col_names, ...) %>%
    dplyr::mutate(direc = ifelse(lead(pot)-pot > 0, "pos", "neg")) %>%
    dplyr::mutate(change = ifelse(direc != lag(direc), 1, 0)) %>%
    dplyr::mutate(change = ifelse(is.na(change), 0, change)) %>%
    dplyr::mutate(sweep = cumsum(change) + 1) %>%
    dplyr::mutate(cv = ceiling(sweep/2))

  class(data) <- c("cv", class(data))
  data
}
