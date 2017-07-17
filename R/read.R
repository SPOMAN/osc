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

#' Read electrochemistry data
#'
#' Reads *.txt files from the CHI potentiostats.
#' Current supports Cyclic Voltammetry and Bulk Electrolysis with Coulometry
#'
#' @param file Path to the *.txt file from the CHI potentiostat
#' @family electrochemistry
#' @export
#' @examples
#' file <- system.file('extdata/cv/cv_example.txt', package = 'osc')
#' data <- read_elec(elec)
#' plot(data)
#'
#' file <- system.file('extdata/cv/Electrolysis.txt', package = 'osc')
#' data <- read_elec(file)
#' plot(data)
#'

electrochemistry_read <- function(file) {
  type <- get_exp_type(file)
  if (type == "Bulk Electrolysis with Coulometry") {
    data <- electrolysis_read(file, skip = find_data(file))
  } else if (type == "Cyclic Voltammetry") {
    data <- cv_read(file, skip = find_data(file))
  } else {
    return("Unknown experiment type")
  }
  return(data)
}

#' Load cyclic voltammogram
#'
#' Reads *.txt files from the CHI potentiostats. Automatically detects sweep- and cv-number, so these can later be used for colors in plots etc.
#'
#' @importFrom magrittr %>%
#' @param path Path to the CV file (txt)
#' @param skip Number of lines to skip (where does metadata end and the data start?)
#' @param col_names A vector containing the names to be used for the two loaded columms (Default: c("potential", "current"))
#' @keywords cyclic voltammetry, electrochemistry
#' @family cyclic voltammetry, electrochemistry
#' @export
#' @examples
#' file <- system.file('extdata/cv/cv_example.txt', package = 'osc')
#' df <- cv_read(file, skip = 41)
#'
#' file <- system.file('extdata/cv/5cv_example.txt', package = 'osc')
#' df <- cv_read(file, skip = 70)
#' plot(df)

cv_read <- function(file, skip, col_names = c("potential", "current")) {
  data <- readr::read_csv(file, skip = skip, col_names = col_names) %>%
    dplyr::mutate(direc = ifelse(lead(potential)-potential > 0, "pos", "neg")) %>%
    dplyr::mutate(change = ifelse(direc != lag(direc), 1, 0)) %>%
    dplyr::mutate(change = ifelse(is.na(change), 0, change)) %>%
    dplyr::mutate(sweep = cumsum(change) + 1) %>%
    dplyr::mutate(cv = ceiling(sweep/2))

  header <- readr::read_lines(file, n_max = skip-1)
  v <- header[stringr::str_detect(header, pattern = "^(Scan Rate)") == TRUE] %>%
    stringr::str_extract(pattern = "-?(\\d)+.(\\d)+$") %>%
    as.numeric()

  structure(list(data = data, v = v), class = "cv")
}

#' Load electrolysis
#' Reads *.txt files from the CHI potentiostats containing electrolysis data.
#'
#' @importFrom magrittr %>%
#' @param path Path to the CV file (txt)
#' @param skip Number of lines to skip (where does metadata end and the data start?)
#' @param col_names A vector containing the names to be used for the two loaded columms (Default: c('time', 'charge', 'current'))
#' @keywords cyclic voltammetry, electrochemistry
#' @family cyclic voltammetry, electrochemistry
#' @export
#' @examples
#' file <- system.file('extdata/cv/cv_example.txt', package = 'osc')
#' df <- cv_read(file, skip = 41)
#'
#' file <- system.file('extdata/cv/5cv_example.txt', package = 'osc')
#' df <- cv_read(file, skip = 70)
#' plot(df)

electrolysis_read <- function(file, skip, col_names = c('time', 'charge', 'current')) {
  data <- readr::read_csv(file, skip = skip, col_names = col_names)

  header <- readr::read_lines(file, n_max = skip-1)
  E <- header[stringr::str_detect(header, pattern = "^(Init E)") == TRUE] %>%
    stringr::str_extract(pattern = "-?(\\d)+.(\\d)+$") %>%
    as.numeric()

  structure(list(data = data, E = E), class = "electrolysis")
}

#' Return electrochemical experiment type
#'
#' @param file Path to data file from an electrochemical experiment

get_exp_type <- function(file) {
  readr::read_lines(file, n_max = 2)[2]
}

#' Find the data startposition
#'
#' Finds the starting point of the data table in data files from electrochemical experiments
#' \code{n_init} is recursively doubled until the starting point is found.
#'
#' @param file Path to data file from an electrochemical experiment
#' @param n_init Initial number of lines to load (the starting point varies from file to file)

find_data <- function(file, n_init = 25) {
  if (n_init > 1000) stop("Start of data not found in the first 1000 lines")
  data_header <- readr::read_lines(file, n_max = n_init) %>%
    stringr::str_detect("(^Time)|(^Potential)") # Time is the first column of coloumetry-data, Potential is the first of CVs
  if(!any(data_header)) {
    return(find_data(file, n_init = n_init * 2))
  } else {
    return(which(data_header == TRUE))
  }
}
