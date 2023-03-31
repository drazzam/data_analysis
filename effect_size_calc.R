# -------------------- For Binary Data --------------------- #

# Calculate the effect size (log risk ratio) and its standard error
effect_sizes <- escalc(measure = "OR", ai = dataset$event1, n1i = dataset$total1, ci = dataset$event2, n2i = dataset$total2)

# Add the effect sizes and their standard errors to the dataset
dataset$effect_size <- effect_sizes$yi
dataset$se_effect_size <- effect_sizes$vi

# View the dataset with the added effect sizes and standard errors
print(dataset)

# -------------------- For Continuous Data --------------------- #
