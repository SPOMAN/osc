#' Title
#'
#' @param df
#' @param source
#' @param output
#'
#' @return
#' @export
#'
#' @examples
echem_calibrate <- function(df, source = "", output = "SCE") {
  if (df$ref == "" & source == "") stop("Please specify reference electrode used in experiment")
}
