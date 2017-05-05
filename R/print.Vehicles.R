#' @export
print.Vehicles <- function(veh, ...) {
  cat("Result for Vehicles ")
    print(unclass(veh),  ...)
}

