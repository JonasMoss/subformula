#' Apply formulas to a model function
#'
#' @export
#' @param formulas A subformulas object.
#' @param model A model function. Should take a formula as its first argument.
#' @param data An optional data frame containing the covariates and response
#' referenced in the formulas supplied. The data argument overrides the environments
#'
#' @param ... Ellipses are passed to the model function.
#' @details If the supplied formula includes the term "0" or "-1", none
#' of the subformulas will include the intercept. Otherwise, the intercept
#' will be interpreted as being protected.
#'
#' @return A list of class "formulas".
#' @examples
#' data(mtcars)
#' forms = subformulas(mpg ~ cyl + disp + hp + drat,
#'                     protected = ~ cyl)
#' mods = fapply(formulas = forms,
#'               model = lm,
#'               data = mtcars)

fapply = function(formulas, model, data, ...) {

  formulas %>%
    lapply(function(form) {
      quote(model(form, data = data, ...)) -> model_call
      form -> model_call[[2]]
      model_call %>% eval
    }) ->
    models



  return(models)

}
