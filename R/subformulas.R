# =============================================================================
# Formula functions.
#
# Methods for creating subformulas of an object.
#
# =============================================================================

#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

protected_to_formula_format = function(vec) {
  vec %>%
    c(sep = " + ") %>%
    as.list %>%
    do.call(paste, .) %>%
    paste("~", .)
}

get_formula_response = function(formula) {

  if (class(formula) != "formula") stop("formula must be of class 'formula'")

  utils::head(all.vars(terms(formula)), n = 1)

}

get_formula_terms = function(formula, data = NULL) {

  if (class(formula) != "formula")  stop("formula must be of class 'formula'")

  attr(terms(formula, data = data), "term.labels")

}

sieve_protected = function(protected, terms) {

  if(class(protected) == "formula") {
    protected %<>% get_formula_terms
  }

  protected = protected[protected %in% terms]

  if(length(protected) == 0) {
    protected = NULL
  }

  protected
}

terms_matrix = function(formula, protected = NULL,
                          keep_intercept = TRUE,
                          keep_interactions = TRUE,
                          data = NULL) {

  formula %<>% as.formula

  formula %>%
    get_formula_response ->
    response

  formula %>%
    get_formula_terms(data) ->
    terms

  protected %<>% sieve_protected(terms)

  length(protected) -> prot_len
  length(terms) - prot_len -> mat_len
  setdiff(terms,protected) -> kept_terms

  rep(TRUE, prot_len*2^mat_len) %>%
    matrix(nrow = 2^mat_len) ->
    protected_matrix

  rep(FALSE, mat_len) %>%
    lapply(. %>% c(TRUE)) %>%
    expand.grid %>%
    cbind(protected_matrix,.) %>%
    magrittr::set_colnames(c(protected,kept_terms)) %>%
    magrittr::extract( , match(terms, names(.))) ->
    term_matrix

  # This part is for the special case when the intercept is removed.
  formula %>%
    terms(data = data) %>%
    attr(., "intercept") ->
    intercept

  if(intercept == 0) {
    term_matrix %<>%
      cbind(., "0" = rep(TRUE,2^mat_len))
  }

  term_matrix

}

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

subformulas = function(formula, protected = NULL, as_formula = TRUE,
                       keep_interactions = TRUE, intercept_protected = TRUE,
                       data = NULL) {

  formula %<>% as.formula

  formula %>%
    get_formula_response ->
    response

  formula %>%
    get_formula_terms(data) ->
    terms

  protected %<>% sieve_protected(terms)

  formula %>%
    terms(data = data) %>%
    attr(., "intercept") ->
    intercept

  if(intercept == 0) {
    terms %<>% c(.,"0")
  }

  terms_matrix(formula = formula,
               protected = protected,
               data = data) ->
    term_matrix_

  term_matrix_ %>%
    apply(1, . %>% terms[.]) %>%
    lapply(., function(x) do.call(paste,
      as.list(c(unlist(x), sep = " + ")))) %>%
    lapply(., function(x) paste0(response, " ~ ", x)) ->
    forms

  if(is.null(protected)) {
    if(intercept == 1){
      forms[[1]] = paste0(response, " ~ 1")
    } else {
      forms[[1]] = NULL
    }
  }

  if (as_formula) {
    parent_environment = attributes(formula)$.Environment
    forms %<>% lapply(as.formula, parent_environment)
  }

  class(forms) = c("subformulas", "list")
  attr(forms, "parent_formula") = formula
  attr(forms, "protected") = protected
  attr(forms, "keep_interactions") = keep_interactions
  attr(forms, "intercept_protected") = intercept_protected
  attr(forms, "call") = match.call()
  attr(forms, "term_matrix") = term_matrix_

  forms

}

