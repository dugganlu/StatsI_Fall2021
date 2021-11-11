##### Question One.

### 1.

results <- lm(voteshare ~ difflog, data = incumbents_subset)

coef(results)

### 2. 

plot(incumbents_subset$difflog, incumbents_subset$voteshare)

abline(lm(voteshare ~ difflog, data = incumbents_subset), col = "red")

### 3.

residuals <- resid(results)

### 4.

"y = (0.57903071) + (0.04166632)x"

##### Question Two

### 1. 

results_1 <- lm(presvote ~ difflog, data = incumbents_subset)

coef(results_1)

### 2.

plot(incumbents_subset$difflog, incumbents_subset$presvote)

abline(lm(presvote ~ difflog, data = incumbents_subset), col = "red")

### 3. 

residuals_1 <- resid(results_1)

### 4.

"y = (0.50758333) + (0.02383723)x"

##### Question Three

### 1.

results_2 <- lm(voteshare ~ presvote, data = incumbents_subset)

coef(results_2)

### 2.

plot(incumbents_subset$voteshare, incumbents_subset$presvote)

abline(lm(voteshare ~ presvote, data = incumbents_subset), col = "red")


### 3.

"y = (0.4413299) + (0.3880184)x"


##### Question Four

### 1. 
results_3 <- lm(residuals ~ residuals_1, data = incumbents_subset)

coef(results_3)

### 2.

plot(residuals, residuals_1)

abline(lm(residuals ~ residuals_1, data = incumbents_subset), col = "red")


### 3.

"y = (-4.859631e-18) +  (2.568770e-01)x"

##### Question Five

### 1.

results_4 <- lm(voteshare ~ difflog + presvote, data = incumbents_subset)

coef(results_4)

### 2.

"y = (0.44864422) +  (0.03554309)x1 + (0.25687701)x2"

### 3. 

"The coefficient on presvote is identical to the coefficient on residuals"