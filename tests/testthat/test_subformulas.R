context("subformulas")

expect_error(subformulas(1))

set.seed(10)
formula = z ~ x + y + t + u - 1
protected = z ~ y + u + v
subs_all = subformulas(formula, protected = protected)
subs_protected = subformulas(formula)

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


## Check generics

expect_equal(print(subs_protected), subs_protected)
expect_equal(print(subs_all), subs_all)
subs = subformulas(formula = z ~ x + y + t + u)
expect_equal(print(subs), subs)
expect_equal(length(subs_protected), length(as.list(subs_protected)))
