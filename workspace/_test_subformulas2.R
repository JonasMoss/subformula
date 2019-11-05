
data(mtcars)
forms = subformulas(mpg ~ .,
                    protected = ~ cyl, data = mtcars)

mods = model_combinations(formulas = forms, model = lm, data = mtcars)

center = function(x) {
  (x - mean(x))/sd(x)
}

mods %>%
  sapply(function(x) summary(x)$adj.r.squared) %>%
  center %>%
  plot(ylim = c(-3,3))

mods %>%
  sapply(., AIC) %>%
  multiply_by(-1) %>%
  center %>%
  points(col = "red")
