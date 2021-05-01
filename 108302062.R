
# read data and save as .csv
csv1 <- read.csv("criminal_analysis_revised.csv")
df <- as.data.frame(csv1, row.names = NULL)
df <- df[-16, ]

# OLS linear regression
result <- lm(log(cases)~log(kmtPercentage) + log(unemploymentRate) +
gun + knife + gunShot + petEnrollment, data = df)
summary(result)

# BP test & White test:
install.packages("lmtest")
install.packages("skedastic")
library(lmtest)
library(skedastic)

bptest(result)
white_lm(result)

# test for multicollinearity
library(car)
vif(result)

# linear hypothesis
# library(car)
linearHypothesis(result, "gun = log(unemploymentRate)")
linearHypothesis(result, "gun = petEnrollment")
linearHypothesis(result, "gun = gunShot")
linearHypothesis(result, "gun = knife")
linearHypothesis(result, "gun = log(kmtPercentage)")

# plot
par(mfrow = c(2, 3))

plot(log(df$kmtPercentage), log(df$cases), type = "p",
xlab = "國民黨在台中市議會所占比例(取對數)", ylab = "刑案發生數")
abline(lsfit(log(df$kmtPercentage), log(df$cases)), col = "red")

plot(log(df$unemploymentRate), log(df$cases), type = "p",
xlab = "失業率(取對數)", ylab = "刑案發生數")
abline(lsfit(log(df$unemploymentRate), log(df$cases)), col = "red")

plot(df$gun, log(df$cases), type = "p",
xlab = "槍枝持有", ylab = "刑案發生數")
abline(lsfit(df$gun, log(df$cases)), col = "red")

plot(df$gunShot, log(df$cases), type = "p",
xlab = "子彈持有", ylab = "刑案發生數")
abline(lsfit(df$gunShot, log(df$cases)), col = "red")

plot(df$knife, log(df$cases), type = "p",
xlab = "刀械持有", ylab = "刑案發生數")
abline(lsfit(df$knife, log(df$cases)), col = "red")

plot(df$petEnrollment, log(df$cases), type = "p",
xlab = "寵物登記數", ylab = "刑案發生數")
abline(lsfit(df$petEnrollment, log(df$cases)), col = "red")