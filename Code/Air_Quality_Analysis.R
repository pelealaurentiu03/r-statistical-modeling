#Loading the data
data("airquality")

#Checking for missing values
summary(airquality)

#Removing rows with missing values
aq_clean <- na.omit(airquality)

#Converting the month to a factor
aq_clean$Month <- as.factor(aq_clean$Month)

summary(aq_clean)

# Standard deviation for ozone and temperature
sd(aq_clean$Ozone)
sd(aq_clean$Temp)

#Creating the visualizations
hist(aq_clean$Ozone,
     main = "Histogram of ozone levels",
     xlab = "Ozone",
     col = "skyblue")

boxplot(Ozone ~ Month, data = aq_clean,
        main = "Ozone levels by month",
        xlab = "Month",
        ylab = "Ozone",
        col = "orange",
        names = c("May", "Jun", "Jul", "Aug", "Sep"))

#Correlation matrix for the significant variables
cor(aq_clean[, 1:4])

#Pairwise scatterplots
pairs(aq_clean[, 1:4], main = "Air quality data scatterplot matrix", col = "blue")

#Hypothesis testing

#Anova test
print("ANOVA results:")
anova_model <- aov(Ozone ~ Month, data = aq_clean)
summary(anova_model)

#Nonparametric test
print("Nonparametric test results:")
kruskal.test(Ozone ~ Month, data = aq_clean)

#Testing the distribution of the data
print("Distribution of the data test results:")
shapiro.test(aq_clean$Ozone)

#Test for proportions
may_aug <- subset(aq_clean, Month %in% c(5, 8))
may_aug$Month <- droplevels(may_aug$Month)

#Creating a contingency table to compare the month and high temp variables
prop_table <- table(may_aug$Month, may_aug$Temp > 80)
print("Table for proportions:")
print(prop_table)

#Runing the proportion test
print("Proportion test results:")
prop.test(prop_table)

#Runing t-test
t.test(Temp ~ Month, data = may_aug)

#Creating the predictive model

#Predicting the ozone level using the other 3 variables
model <- lm(Ozone ~ Solar.R + Wind + Temp, data = aq_clean)

summary(model)

#Predicting the ozone level for a random day
new_data <- data.frame(Solar.R = 190, Wind = 7.4, Temp = 80)

prediction <- predict(model, newdata = new_data)
print(paste("Predicted ozone level:", round(prediction, 2)))

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))