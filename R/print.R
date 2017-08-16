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
