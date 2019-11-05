#' @export
print.subformulas = function(x, ...) {

  xname = deparse(match.call()[[2]])
  dots = list(...)

  if (is.null(dots$show)) show = 10

  cat("\nSubformulas list \n")

  attr(x, "parent_formula") %>%
    as.character ->
    formula_chars

  cat("\n Parent formula:       ",
        formula_chars)

  if (!is.null(attr(x,"protected"))) {
    cat("\n Protected covariates: ",
        attr(x,"protected") %>%
          protected_to_formula_format)
  }

  cat("\n Number of subformulas:",
        length(x))

  if(!attr(x, "intercept_protected")) {
    cat("\n Intercept protected:  ",
          attr(x, "intercept_protected"))
  }

  if(!attr(x, "keep_interactions")) {
  cat("\n Interactions kept:    ",
        attr(x, "keep_interactions"))
  }

  cat(paste0("\n\nUse as.list(",xname,") to show all subformulas.\n\n"))
}

#' @export
print.models = function(x, ...) {

  dots = list(...)

  if (is.null(dots$show)) show = 10

  print.default(x)

}

#' @export
as.list.subformulas = function(x, ...) {
  new_x = x
  attributes(new_x) = NULL
  new_x
}

#' @export
as.list.models = function(x, ...) {
  new_x = x
  attributes(new_x) = NULL
  new_x
}


#' Extracts fitted models from a "models" object. Supports indexation
#' integers.
#'
#' @export
#' @param x An object of class "subformula" or integer vector.
#'
#' @return A subformulas object.
#
`[.subformulas` = function(forms, x) {
  if("integer"  %in% class(x))  {
    models %>%
      as.list %>%
      extract(x) ->
      new_models

    class(new_models) = c("models")
    attr(new_models, "model") = attr(models, "model")
    attr(models, "formulas") %>%
      extract(x) ->
      attr(new_models, "formulas")

    new_models

  } else if ("subformulas" %in% class(x)) {
    attr(mods, "formulas") %>%
      subformula_indices(., x) ->
      indices

    models %>%
      as.list %>%
      extract(indices) ->
      new_models

    class(new_models) = c("models")
    attr(new_models, "model") = attr(models, "model")
    attr(new_models, "formulas") = x

    new_models
  }
}

#' Allows subformulas by formulas.
#'
#' @export
#' @param obj An object of class "subformula".
#'
#' @return A list.

R.methodsS3::setMethodS3(":", "formula", function(protected, formula, ...) {
  subformulas(formula, protected, ...)
})

#' Extracts fitted models from a "models" object. Supports indexation
#' by formula and integers.
#'
#' @export
#' @param x An object of class "subformula" or integer vector.
#'
#' @return A models object.
#
`[.models` = function(models, x) {
  if("integer"  %in% class(x))  {
    models %>%
      as.list %>%
      extract(x) ->
      new_models

    class(new_models) = c("models")
    attr(new_models, "model") = attr(models, "model")
    attr(models, "formulas") %>%
      extract(x) ->
      attr(new_models, "formulas")

    new_models

  } else if ("subformulas" %in% class(x)) {
    attr(models, "formulas") %>%
      subformula_indices(., x) ->
      indices

    models %>%
      as.list %>%
      extract(indices) ->
      new_models

    class(new_models) = c("models")
    attr(new_models, "model") = attr(models, "model")
    attr(new_models, "formulas") = x

    new_models
  }
}
