### Problem Set #4 - Luke Duggan - 16316834 ###

# Question One #

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

# (a)

Prestige$professional <- ifelse(Prestige$type=="prof", 1, 0)

# (b)

results <- lm(prestige ~ income + professional + income*professional, data = Prestige)

# (c)

results$coefficients

### Question Two

test_statistic_1 <- (0.042)/(0.013)
test_statistic_1

qt(0.95, 27)

test_statistic_2 <- (0.042)/(0.013)
test_statistic_2

qt(0.95, 73)