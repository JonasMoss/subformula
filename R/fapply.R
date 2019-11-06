#' Apply formulas to a model function
#'
#' @export
#' @param formulas an object of class "\code{subformulas}".
#' @param model a function taking a \code{formula} as its first argument.
#' @param ... additional arguments to be passed to the \code{model}.
#' @return A list of evaluated function calls.
#' @examples
#' formulas = subformulas(mpg ~ cyl + disp + hp + drat, protected = ~ cyl)
#' models = fapply(formulas = formulas, model = lm, data = mtcars)

fapply = function(formulas, model, ...) {

  model = substitute(model)

  models = lapply(formulas, function(f) formula_to_call(f, model, ...))

  stats::setNames(models, sapply(as.list(formulas), as.character))

}
