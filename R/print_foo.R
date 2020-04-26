#' @export
print.stockyogo <- function(teste){
  print(paste("Test Statistic is",round(teste$stat,3),"with critical value", teste$critical_value))
}

#' @export
print.moreirateste <- function(test){
  print(paste("Test Statistic is",round(test$stat,3),"p-val", test$p_val))
}