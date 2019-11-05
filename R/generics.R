#' @export
print.subformulas = function(x, ...) {

  xname = deparse(match.call()[[2]])

  cat("\nSubformulas list \n")

  cat("\n Parent formula:       ", as.character(attr(x, "formula")))

  if (!is.null(attr(x, "protected"))) {
    cat("\n Protected covariates: ", attr(x, "protected"))
  }

  cat("\n Number of subformulas:", length(x))

  cat(paste0("\n\nUse as.list(", xname, ") to show all subformulas.\n\n"))

  invisible(x)

}

#' @export
as.list.subformulas = function(x, ...) {

  attributes(x) = NULL
  x

}
