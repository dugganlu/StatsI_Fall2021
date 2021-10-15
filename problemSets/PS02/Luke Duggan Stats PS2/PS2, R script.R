### Question One.

#Calculating Pearson's test statistic.

table <- rbind(c(14, 6, 7), c(7, 7, 1))
A <- addmargins(table)

E11 <- (A[1,4]/42) * (A[3,1]/42) * 42
E21 <- (A[2,4]/42) * (A[3,1]/42) * 42
E12 <- (A[1,4]/42) * (A[3,2]/42) * 42
E22 <- (A[2,4]/42) * (A[3,2]/42) * 42
E13 <- (A[1,4]/42) * (A[3,3]/42) * 42
E23 <- (A[2,4]/42) * (A[3,3]/42) * 42

chi <- ((A[1,1] - E11)^2 / E11) + ((A[2,1] - E21)^2 / E21) + 
  ((A[1,2] - E12)^2 / E12) + ((A[2,2] - E22)^2 / E22) +
  ((A[1,3] - E13)^2 / E13) + ((A[2,3] - E23)^2 / E23)
chi

# Calculating p-value.

pchisq(3.79, 2, lower.tail = FALSE)

### Question Two

dataset <- read.csv(file = "duflodata.txt", header = TRUE)

# R has a built-in function for OLS estimation of a linear model:

results <- lm(dataset$water ~ dataset$reserved)

# We could, however, do this manually:

slope_hat <- (cov(dataset$reserved, dataset$water))/var(dataset$reserved)

intercept_hat <- mean(dataset$water) - slope_hat*mean(dataset$reserved)

# To see that the results are equivalent:
coef(results)
slope_hat
intercept_hat

summary(results)

### Question Three.

### 1. Import the data set, obtain summary statistics, and the overall distribution.

fruitflies <- read.csv("http://stat2.org/datasets/FruitFlies.csv")

# Summary statistics.
summary(fruitflies)

# Distribution
plot(fruitflies$ID, fruitflies$Longevity)

### 2. Plot lifespan vs thorax
plot(fruitflies$Thorax, fruitflies$Longevity)

#Correlation
cor(fruitflies$Longevity, fruitflies$Thorax)

### 3. Regress lifespan on thorax. Interpret the coefficients.

results <- lm(Longevity ~ Thorax, data = fruitflies)

coef(results)

### 4. Test for a significant linear relationship. Provide and interpret results.

# Firstly, we calculate the test statistic manually. The formula is no different
# to a normal t-test: we subtract our estimate from the hypothesized true 
# parameter (in this case 0) and divide by the standard error of the estimate 
# (slight abuse of terminology: we should say the standard error of the 
# *estimator*). 

# Calculating the standard error of the estimator is a bit involved, since it 
# requires deriving the variance of the sampling distribution of the estimator.
# We will make use of the fact that R computes it for us (making this only
# partially a manual computation). To see the standard errors of the estimates:

summary(results)$coefficients[,2]

# Then, the test statistic is just a quotient, following the t-distribution
# with 123 = (125 - 2) degrees of freedom. We see that it greatly exceeds the 
# critical value at the 0.001 level:

144.3331/(15.77045)
qt(0.001, 123, lower.tail = FALSE)

# Alternatively, R actually provides this t-value and the associated p-value:

summary(results)$coefficients[,3]

# Compare the t-values with the critical values at the 0.001 level.

fail_to_reject_region <- c(qt(0.001, 123,),
                           qt(0.001, 123, lower.tail = FALSE))
fail_to_reject_region

# Alternatively, observe the minuscule p-values associated with the null 
# hypotheses that the parameters are 0.

summary(results)$coefficients[,4]

### 5. Provide the 90% confidence interval for the slope of the fitted model.
### Use both the formula and R's built-in command.

# The built-in command is:
confint(results, level=.9)

# Manually; the formula is the point estimate of the parameter plus / minus
# the standard error of the estimate times the t quantile for the appropriate
# level of significance; note especially the degrees of freedom correction,
# the sample size minus 2. Since we have access to these quantities in the
# results of our lm() call, we can plug them in and get.

c("lower" = 144.3331 - qt(0.95, 123)*(15.77045),
  "upper" = 144.3331 + qt(0.95, 123)*(15.77045))


### 6. Predict an individual fruitfly's lifespan when thorax = 0.8.

pred_interval <- predict(results, newdata=data.frame(Thorax=0.8), interval="prediction", level = 0.95)
pred_interval

# Predict the average lifespan when thorax = 0.8.

conf_interval <- predict(results, newdata=data.frame(Thorax=0.8), interval="confidence", level = 0.95)
conf_interval

### 7. For a sequence of thorax values, plot their fitted values for lifespan,
# as well as the prediction and confidence intervals.

plot(fruitflies$Thorax, fruitflies$Longevity)
abline(results, col="red")

# Firstly, let's generate a sequence of thorax values.
sequence <- seq(0.5, 1, by=0.05)

# Then, we plot the confidence intervals generated in the last part.
lines(sequence, conf_interval[2], col="green")
lines(sequence, conf_interval[3], col="green")

# Finally, we plot the prediction intervals. 
lines(sequence, conf_interval[2], col="blue")
lines(sequence, conf_interval[3], col="blue")