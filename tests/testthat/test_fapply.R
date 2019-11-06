context("fapply")

formulas = subformulas(mpg ~ cyl + disp + hp + drat, protected = ~ cyl)
fapply_object = fapply(formulas, model = lm, data = mtcars, x = TRUE)
AICs = sapply(fapply(formulas, model = lm, data = mtcars, x = TRUE), AIC)
expect_equal(length(AICs), 8)
expect_equal(names(fapply_object), sapply(as.list(formulas), as.character))
