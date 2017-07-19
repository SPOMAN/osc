#' Print function for electrolysis
#' @export

print.electrolysis <- function(x) {
  cat(paste0("Electrolysis at ", x$E,
             " V for ", x$d, " s (",
             round(x$d / 3600, digits = 2),
             " h)\n"))
  electrons <- x$Q / constants::syms$e
  mol <- electrons / constants::syms$Na
  cat(x$Q, " C used (", prettyNum(electrons, digits = 3, format = "fg")," electrons \U2248 ", prettyNum(mol, digits = 3, format = "fg") ," mol)\n\n", sep = "")
  print(x$data)
  invisible(x)
}


#' Print function for CV
#' @export

print.cv <- function(x) {
  cat("Cyclic Voltammetry at ", x$v, " V/s\n", sep = "")
  cat("Initial scan polarity: ", x$init_P, ", Scan segments: ", x$seg, "\n", sep = "")
  cat("Init E: ", x$init_E, " V, High E: ", x$high_E, " V, Low E: ", x$low_E, " V\n", sep = "")
  cat("Sensitivity: ", x$sens, ", Quiet time: ", x$quiet, " s\n\n", sep = "")
  print(x$data)
  invisible(x)
}
