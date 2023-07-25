#fit logistic regression model
model <- glm(Retreatement ~ age, family = "binomial", data = data)

#view model summary
summary(model)

#extract coefficient and standard error for age
coef <- summary(model)$coefficients["age", 1]
se <- summary(model)$coefficients["age", 2]

#calculate odds ratio
or <- exp(coef)

#calculate 95% confidence interval
ci <- exp(coef + qnorm(c(0.025, 0.975)) * se)

#print results
cat("P-value:", summary(model)$coefficients["age", 4], "\n")
cat("Odds ratio:", or, "\n")
cat("95% confidence interval:", ci, "\n")
