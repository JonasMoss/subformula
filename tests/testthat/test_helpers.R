context("helpers")


## Input checks
expect_error(subformulas(1))
expect_error(get_formula_terms("string"), "formula must be of class 'formula'")
expect_error(get_formula_response("string"), "formula must be of class 'formula'")

set.seed(10)
formula = z ~ x + y + t + u - 1
protected = z ~ y + u + v

data = data.frame(z = rnorm(10),
                  x = rnorm(10),
                  y = 1:10,
                  t = rnorm(10),
                  u = runif(10))

subs_all = subformulas(formula, protected = protected)
subs_protected = subformulas(formula)

expect_equal(get_formula_response(formula), "z")
expect_equal(get_formula_terms(formula), c("x", "y", "t", "u"))
expect_equal(length(subformulas(formula, protected = protected)), 4)
expect_equal(length(subformulas(formula)), 16 - 1)

formula = z ~ x + y + t + u
protected = z ~ y + u + v
expect_equal(length(subformulas(formula, protected = protected)), 4)
expect_equal(length(subformulas(formula)), 16)

## Check attributes
obj = subformulas(formula, protected = protected)
expect_equal(attr(obj, "protected"), c("y", "u"))
expect_equal(attr(obj, "class"), c("subformulas", "list"))
expect_equal(attr(obj, "formula"), z ~ x + y + t + u)
expect_equal(deparse(attr(obj, "call")),
             "subformulas(formula = formula, protected = protected)")


## Check term matrix

expect_equal(dim(terms_matrix(z ~ x + y + t + u, protected = protected)),
             c(4, 4))
expect_equal(dim(terms_matrix(z ~ x + y + t + I(u^2) + u,
                              protected = c("t","u","w"))), c(8, 5))
expect_equal(dim(terms_matrix(z ~ x + y + t + u)),
             c(16, 4))

## Check generics

expect_equal(print(subs_protected), subs_protected)
expect_equal(print(subs_all), subs_all)
subs = subformulas(formula = z ~ x + y + t + u)
expect_equal(print(subs), subs)
expect_equal(length(subs_protected), length(as.list(subs_protected)))
