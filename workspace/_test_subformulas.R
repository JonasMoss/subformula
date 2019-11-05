library("nlme")
library("magrittr")

set.seed(10)
formula = z ~ x + y + t + u - 1
data = data.frame(z = rnorm(10),
                  x = rnorm(10),
                  y = 1:10,
                  t = rnorm(10),
                  u = runif(10))

get_formula_response(formula)
get_formula_terms(formula)

protected = z ~ y + u + v
as_formula = TRUE

terms_matrix(z ~ x + y + t + u, protected = protected)
terms_matrix(z ~ x + y + t + I(u^2) + u, protected = c("t","u","w"))
terms_matrix(z ~ x + y + t + u)

subformulas(formula, protected = protected)
obj = subformulas(formula, protected = protected)
subs1 = subformulas(formula)
subs2 = obj

subformulas(formula)
model_combinations(formulas = obj, data = data, model = lm)
model_combinations(formulas = obj, data = data, model = glm,
                   family = gaussian())

formula = z ~ x + y + t + I(u^2)
formulas = subformulas(formula)

formula = frequency ~ attitude + gender + (1|subject) + (1|scenario)

data(mtcars)
forms = subformulas(mpg ~ .,
                    protected = ~ cyl, data = mtcars)
mods = model_combinations(formulas = forms, model = lm, data = mtcars)
obj = (mpg ~ cyl):(mpg ~ cyl + disp + drat)
mods[obj]
