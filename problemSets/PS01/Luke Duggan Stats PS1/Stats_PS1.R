###Applied Stats - Problem Set 1 - Luke Duggan###

#Question One#

#We can use a quick command to compute the confidence interval and
#perform the hypothesis test.


y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

results <- t.test(y, mu = 100, conf.level=.90)

print(results)

#Alternatively, we can do this manually. Firstly the confidence interval:

sample_mean <- mean(y)
sample_sd <- sd(y)
t90 <- qt(0.05, 24, lower.tail=FALSE)

lower <- sample_mean - t90*(sample_sd/sqrt(25))
upper <- sample_mean + t90*(sample_sd/sqrt(25))
CI <- c(lower, upper)

print(CI)

#And then the value of the test statistic:

t <- (sample_mean - 100)/(sample_sd/sqrt(25))

print(t)

#To find the critical values for a two-tailed test at the 0.05 significance level:
print(qt(0.025, 24))
print(qt(0.975,24))


#Question Two#

#A. We load the data and generate all two way scatterplots.
expenditure <- read.delim("C:/Users/Luke Duggan/Downloads/expenditure.txt", row.names=1)
   View(expenditure)
plot(expenditure)

#B. We now plot Region against expenditure per capita on homelessness.
plot(expenditure$Region, expenditure$Y, type=h)

#C. We now plot income per capita in a state against expenditure per capita on
#homelessness, and then alter the graph as requested.
plot(expenditure$X1, expenditure$Y)

#Then we subset our data.
expenditure.s1 <- expenditure[expenditure$Region ==1]
expenditure.s1 <- expenditure[expenditure$Region ==1,]
expenditure.s2 <- expenditure[expenditure$Region ==2,]
expenditure.s3 <- expenditure[expenditure$Region ==3,]
expenditure.s4 <- expenditure[expenditure$Region ==4,]

#Finally, we plot our graphs and overlay them.
plot(expenditure.s1$X1, expenditure.s1$Y, pch=8, col="red")
points(expenditure.s2$X1, expenditure.s2$Y, pch=9, col="blue")
points(expenditure.s3$X1, expenditure.s3$Y, pch=10, col="green")
points(expenditure.s4$X1, expenditure.s4$Y, pch=11, col="gold")