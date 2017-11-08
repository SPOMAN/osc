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

echem_read <- function(file, type = NA) {
  if (is.na(type)) type <- get_exp_type(file)
  if (is.na(type)) stop("Unknown experiment type. Please specify using the type parameter.")

  if (type == "Electrolysis") {
    data <- electrolysis_read(file, skip = find_data(file))
  } else if (type == "CV") {
    data <- cv_read(file, skip = find_data(file))
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
  # data <- readr::read_csv(file, skip = skip, col_names = col_names)
  #
  # if (dim(readr::problems(data))[1] != 0) {
  #   # Something when wrong while loading the data.
  #   # This is probably due to 'Segment...' in the middle of the data.
  #   data <- data %>%
  #     dplyr::filter(!stringr::str_detect(potential, "^Segment")) %>%
  #     dplyr::mutate_all(dplyr::funs(as.numeric(.)))
  # }
  data <- tibble::tibble(data = readr::read_lines(file, skip = skip)) %>%
    tidyr::separate(data, into = col_names, sep = ",", fill = "left") %>%
    dplyr::filter(stringr::str_detect(potential, "^-*[\\d]+")) %>%
    dplyr::mutate_all(dplyr::funs(as.numeric(.)))

  data <- data %>%
    dplyr::mutate(direc = ifelse(lead(potential)-potential > 0, "pos", "neg")) %>%
    dplyr::mutate(change = ifelse(direc != lag(direc), 1, 0)) %>%
    dplyr::mutate(change = ifelse(is.na(change), 0, change)) %>%
    dplyr::mutate(sweep = cumsum(change) + 1) %>%
    dplyr::mutate(cv = ceiling(sweep/2))

  header <- readr::read_lines(file, n_max = skip-1)
  attr(data, "meta") <- list(
    scanrate = extract_metadata(header, "(Scan Rate)|(v)"),
    init_E = extract_metadata(header, "(Init E)|(Estart)"),
    high_E = extract_metadata(header, "(High E)|(Eend)"),
    low_E = extract_metadata(header, "(Low E)|(Eswitch)"),
    init_P = extract_metadata(header, "Init P", pattern = "(P|N)$", numeric = FALSE),
    seg = extract_metadata(header, "Segment\\s="),
    sens = extract_metadata(header, "Sensitivity"),
    quiet = extract_metadata(header, "Quiet Time")
  )
  class(data) <- append("cv", class(data))
  data
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
  d <- dplyr::last(data$time)
  Q <- dplyr::last(data$charge)

  attr(data, "meta") <- list(
    E = E,
    d = d,
    Q = Q)
  class(data) <- append("electrolysis", class(data))
  data
}

#' Return electrochemical experiment type
#'
#' @param file Path to data file from an electrochemical experiment

get_exp_type <- function(file) {
  exp_type <- readr::read_lines(file, n_max = 3)

  dplyr::case_when(
    exp_type[2] == 'Cyclic Voltammetry' ~ 'CV',
    exp_type[2] == 'Bulk Electrolysis with Coulometry' ~ 'Electrolysis',
    exp_type[3] == 'file type: CV' ~ 'CV',
    TRUE ~ NA_character_
  )
}

#' Find the data startposition
#'
#' Finds the starting point of the data table in data files from electrochemical experiments
#' \code{n_init} is recursively doubled until the starting point is found.
#'
#' @param file Path to data file from an electrochemical experiment
#' @param n_init Initial number of lines to load (the starting point varies from file to file)

find_data <- function(file, n_init = 25) {

  if (n_init > R.utils::countLines(file)) stop("Start of data not found anywhere in the file.")
  data_header <- readr::read_lines(file, n_max = n_init) %>%
    stringr::str_detect("^-?[\\d]+") # The first line of the data is always the first line to start with a digit

  if(!any(data_header)) {
    return(find_data(file, n_init = n_init * 2))
  } else {
    return(which(data_header == TRUE)[1] - 1)
  }
}

#' Extract metadata from the header of a datafile
#'
#' @param header A vector of header lines in a datafile
#' @param start A string with the beginning of the line of interest
#' @param pattern Regular expression detailing how to extract the value. The default will attempt to match any number.
#' @param numeric Should the function return a numeric (TRUE) or character (FALSE)
#'
extract_metadata <- function(header, start, pattern = "[-+]?\\d+\\.?\\d*e?-?\\d*", numeric = TRUE) {
  start <- paste0("^(", start, ")")

  metadata <- header[stringr::str_detect(header, pattern = start) == TRUE] %>%
    stringr::str_extract(pattern = pattern)

  if (numeric) metadata <- metadata %>% as.numeric()

  metadata
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
    ggplot2::labs(title = paste("Electrolysis at ", meta(df)$E, " V"), x = "Time (s)", y = "Current (A)")
}


#' Print function for electrolysis
#' @export

print.electrolysis <- function(x) {
  cat(paste0("Electrolysis at ", meta(x)$E,
    " V for ", meta(x)$d, " s (",
    round(meta(x)$d / 3600, digits = 2),
    " h)\n"))
  electrons <- meta(x)$Q / constants::syms$e
  mol <- electrons / constants::syms$Na
  cat(meta(x)$Q, " C used (", prettyNum(electrons, digits = 3, format = "fg")," electrons \U2248 ", prettyNum(mol, digits = 3, format = "fg") ," mol)\n\n", sep = "")

  print(tibble::as.tibble(unclass(x)), n = 5)
  invisible(x)
}


#' Print function for CV
#' @export

print.cv <- function(x) {
  cat("Cyclic Voltammetry at ", meta(x)$scanrate, " V/s\n", sep = "")
  cat("Initial scan polarity: ", meta(x)$init_P, ", Scan segments: ", meta(x)$seg, "\n", sep = "")
  cat("Init E: ", meta(x)$init_E, " V, High E: ", meta(x)$high_E, " V, Low E: ", meta(x)$low_E, " V\n", sep = "")
  cat("Sensitivity: ", meta(x)$sens, ", Quiet time: ", meta(x)$quiet, " s\n\n", sep = "")

  print(tibble::as.tibble(unclass(x)), n = 5)
  invisible(x)
}
