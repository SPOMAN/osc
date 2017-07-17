#' Print function for electrolysis
#' @export

print.electrolysis <- function(df) {
  cat("Electrolysis at ", df$E, " V\n\n", sep = "")
  print(df$data)
}


#' Print function for CV
#' @export

print.cv <- function(df) {
  cat("Cyclic Voltammetry at ", df$v, " V/s\n\n", sep = "")
  print(df$data)
}
