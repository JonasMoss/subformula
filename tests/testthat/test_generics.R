context("generics")

formula = z ~ x + y + t + u - 1
protected = z ~ y + u + v
subs_all = subformulas(formula, protected = protected)
subs_protected = subformulas(formula)
subs = subformulas(formula = z ~ x + y + t + u)

expect_equal(print(subs_protected), subs_protected)
expect_equal(print(subs_all), subs_all)
expect_equal(print(subs), subs)

expect_equal(length(subs_protected), length(as.list(subs_protected)))
