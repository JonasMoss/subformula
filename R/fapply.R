#' Apply Formulas to a Model
#'
#' This is a member of the \code{apply} family. It is similar to
#' \code{lapply}, but handles the \code{call} slightly differently.
#' This makes the output prettier.
#'
#' @export
#' @param formulas a list of formulas or objects coercible to formula by
#'    \code{\link[stats]{stats::as.formula}}.
#' @param model a function taking a \code{formula} as its first argument.
#' @param ... additional arguments to be passed to \code{model}.
#' @return \code{fapply} returns a list of evaluated function calls.
#' @examples
#' formulas = subformulas(mpg ~ cyl + disp, protected = ~ cyl)
#' fapply(formulas, lm, data = mtcars) # Pretty output.
#' lapply(formulas, lm, data = mtcars) # Less pretty output.

fapply = function(formulas, model, ...) {

  model = substitute(model)

  formulas = lapply(formulas, stats::as.formula)

  models = lapply(formulas, function(f) formula_to_call(f, model, ...))

  stats::setNames(models, sapply(formulas, formula_to_character))

}
