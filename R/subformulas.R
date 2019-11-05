#' Find subformulas of a formula with protected covariates.
#'
#' @export
#' @param formula A formula or string specifying a formula.
#' @param protected A vector or formula specifying which, if any,
#' covariates are protected. They will appear in all computed
#' subformulas.
#' @param as_formula Bool specifying whether the function returns
#' objects of class formula or string.
#' @param keep_interactions If true, interaction terms will only appear
#' together with their parents: For x:y to appear, x and y must also appear.
#' @param data Optional supplied data. Is used to fill out formulas as in
#' "y ~ .".
#' @details If the supplied formula includes the term "0" or "-1", none
#' of the subformulas will include the intercept. Otherwise, the intercept
#' will be interpreted as being protected.
#'
#' @return A list of class "formulas".
#' @examples
#' subformulas(z ~ x + y)
#' subformulas(y ~ x + y + y^2, protected = ~ x)
#' subformulas(y ~ x + y + t + I(t^2), protected = c("x","I(t^2)"))

subformulas = function(formula,
                       protected = NULL,
                       as_formula = TRUE,
                       data = NULL) {

  formula = as.formula(formula)
  response = get_formula_response(formula)
  terms = get_formula_terms(formula, data)
  protected = get_protected(protected, terms)
  intercept = get_intercept(formula, data)

  term_matrix_ = terms_matrix(formula = formula,
                              protected = protected,
                              data = data)

  forms = apply(term_matrix_, 1, function(x) terms[x])
  forms = lapply(forms, function(x)
    do.call(paste, as.list(c(unlist(x), sep = " + "))))
  forms = lapply(forms, function(x) paste0(response, " ~ ", x))

  if (is.null(protected))
    forms[[1]] = if (intercept == 1) paste0(response, " ~ 1") else NULL

  if (as_formula)
    forms = lapply(forms, as.formula, attributes(formula)$.Environment)

  class(forms) = c("subformulas", "list")
  attr(forms, "formula") = formula
  attr(forms, "protected") = protected
  attr(forms, "call") = match.call()
  attr(forms, "term_matrix") = term_matrix_

  forms

}
