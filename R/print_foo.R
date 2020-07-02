#' @export
#' @import glue
print_stockyogo <- function(teste) {
  (glue("Test Statistic is {round(teste$stat, 3)} with critical value {teste$critical_value}"))
}

#' @export
#' @import glue
print.moreirateste <- function(test) {
  (glue("Test Statistic is {round(test$stat, 3)} --- p-val is {test$p_val}"))
}