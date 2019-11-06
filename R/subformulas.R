#' Find subformulas of a formula with protected covariates.
#'
#' @export
#' @param formula A formula or string specifying a formula.
#' @param protected A vector or formula specifying which, if any,
#' covariates are protected. They will appear in all computed
#' subformulas.
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
                       data = NULL) {

  formula = stats::as.formula(formula)
  response = get_formula_response(formula)
  terms = get_formula_terms(formula, data)
  protected = get_protected(protected, terms)
  intercept = get_intercept(formula, data)

  term_matrix_ = terms_matrix(formula = formula,
                              protected = protected,
                              data = data)

  if (intercept == 0) {
    term_matrix_ = cbind(term_matrix_, rep(TRUE, nrow(term_matrix_)))
    colnames(term_matrix_)[length(terms) + 1] = intercept
    forms = apply(term_matrix_, 1, function(x) c(terms, 0)[x])
  } else {
    forms = apply(term_matrix_, 1, function(x) terms[x])
  }


  forms = lapply(forms, function(x)
    do.call(paste, as.list(c(unlist(x), sep = " + "))))
  forms = lapply(forms, function(x) paste0(response, " ~ ", x))

  if (is.null(protected))
    forms[[1]] = if (intercept == 1) paste0(response, " ~ 1") else NULL

  class(forms) = c("subformulas", "list")
  attr(forms, "formula") = formula
  attr(forms, "protected") = protected
  attr(forms, "call") = match.call()
  attr(forms, "term_matrix") = term_matrix_

  forms

}
