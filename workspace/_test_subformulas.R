

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





data

formulas = subformulas(mpg ~ cyl + disp + hp + drat - 1, protected = ~ cyl)
forms = formulas

fapply(formulas, model = lm, data = mtcars)
fapply(forms, model = lm, data = mtcars, x = TRUE)

formula_to_call(formulas[[1]], lm, data)

f = function(x) {
  g = function(x) substitute(x)
  g(x)
}
