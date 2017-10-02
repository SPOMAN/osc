#' Lorentzian curve
#'
#' Generates a Lorentzian curve based on the input data.
#' @references \url{http://mathworld.wolfram.com/LorentzianFunction.html}
#'
#' @param x Point along x-axis, e.g. the wavenumber
#' @param G Full-width at half max of the peak
#' @param x_0 Center of the peak
#' @param A Area of the peak
#'
#' @examples
#' x <- 1:1000
#' plot(x, gr_lorentzian(x, G = 10, x_0 = 500, A = 10))
#'
#' @export

gr_lorentzian <- function(x, G, x_0, A) {
  A * (1/pi) * (0.5 * G) / ((x - x_0)^2 + (0.5 * G)^2)
}

#' Gaussian curve
#'
#' Generates a Gaussian curve based on the input data.
#'
#' @references \url{http://mathworld.wolfram.com/GaussianFunction.html}
#'
#' @inheritParams gr_lorentzian
#'
#' @examples
#' x <- 1:1000
#' plot(x, gr_gaussian(x, G = 10, x_0 = 500, A = 10))
#'
#' @export

gr_gaussian <- function(x, G, x_0, A) {
  s = G / 2.3548
  A * (1) / (s * sqrt(2*pi)) * exp(-(x - x_0)^2 / (2 * s^2))
}

#' Return Lorentzian or Gaussian fit
#'
#' Internal function, used by `gr_cf_fit_add`
#'
#' @param type Either "Lorentzian" or "Gaussian"
#' @param perc_gauss 0-100 value indicating the amount of gaussian in a curve of type 'Mixed'
#' @param ... Arguments passed on to gr_lorentzian() or gr_gaussian()
#'
#' @export
#'
gr_fit_line <- function(type, perc_gauss = 0, ...) {
  if (type == "Lorentzian") {
    return(gr_lorentzian(...))
  } else if (type == "Gaussian") {
    return(gr_gaussian(...))
  } else if (type == "Mixed") {
    p_gauss = perc_gauss / 100
    mix <- (1 - p_gauss) * gr_lorentzian(...) + (p_gauss) * gr_gaussian(...)
    return(mix)
  }
}

#' Add peaks to a spectrum data frame
#'
#' Supplied with a spectrum as a tab-separated text-file with wavenumber and intensites, and a curvefit-results table from Wire, it will generate columns for each peak with the corresponding intensities, for easy plotting. The envelope of the fit is also calculated by summing all peaks at each wavenumber.
#'
#' @importFrom magrittr %>%
#' @param spectrum_file Path to a tab-separated file with a spectrum in two columns (wavenumber and intensity)
#' @param cf_file Path to a file containing a curvefit results table (made in Wire by right-clicking results of a curvefit and choosing Copy Results)
#' @family raman fits
#'
#' @examples
#' curvefit <- system.file("extdata/curvefit_data.txt", package = "gRaphene")
#' spectrum <- system.file("extdata/spectrum_example.txt", package = "gRaphene")
#'
#' gr_cf_fit_add(spectrum, curvefit)
#'
#' @export

gr_cf_fit_add <- function(spectrum_file, cf_file) {
  cf <- readr::read_tsv(cf_file)
  spec <- readr::read_tsv(spectrum_file, col_names = c('wavenumber', 'intensity'), skip = 1)# %>% dplyr::select(-index)

  for (i in seq_along(cf$`Curve Name`)) {
    spec[, cf$`Curve Name`[i]] = gr_fit_line(type = cf$Type[i], perc_gauss = cf$`% Gaussian`[i], x = spec$wavenumber, G = cf$Width[i], x_0 = cf$Centre[i], A = cf$Area[i])
  }

  spec <- spec %>%
    dplyr::mutate(envelope = rowSums(.[-(1:2)]))
  spec
}

#' Plot fitted spectrum with peaks
#'
#' Takes a spectrum and corresponding curvefit results and generates a plot
#'
#' @importFrom magrittr %>%
#' @inheritParams gr_cf_fit_add
#' @family raman fits
#'
#' @examples
#'
#' curvefit <- system.file("extdata/curvefit_data.txt", package = "gRaphene")
#' spectrum <- system.file("extdata/spectrum_example.txt", package = "gRaphene")
#'
#' gr_cf_fit_plot(spectrum, curvefit)
#'
#' @export

gr_cf_fit_plot <- function(spec, curvefit) {
  gr_cf_fit_add(spec, curvefit) %>%
    tidyr::gather(key = "peak", value = "int", -wavenumber, -intensity, -envelope) %>%
    ggplot2::ggplot(ggplot2::aes(x = wavenumber, y = intensity)) +
    ggplot2::geom_line(ggplot2::aes(y = envelope), size = 1) +
    ggplot2::geom_point(size = 0.1, color = "dark grey") +
    ggplot2::geom_ribbon(ggplot2::aes(ymax = int, fill = peak), ymin = 0, alpha = 0.75) +
    ggplot2::scale_fill_brewer(palette = "Paired", guide = ggplot2::guide_legend(title = "Peaks")) +
    ggplot2::labs(x = expression(Wavenumber~(cm^{-1})), y = "Intensity")
}
