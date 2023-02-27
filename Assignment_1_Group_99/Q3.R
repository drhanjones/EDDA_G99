library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape)
library(ggpubr)
library(car)

q <- "Assignment_1_Group_99/datasets/diet.txt"
diet_raw <- read.csv(q, header = TRUE, sep = "",  dec = ".", check.names = TRUE)


clean_data <- function(data){

  data <- na.omit(data) #Removing rows with NA
  data <- data[, colSums(is.na(data)) == 0]
  return(data)
}


diet_clean <- clean_data(diet_raw)
diet_clean$weight_lost <- diet_clean$preweight - diet_clean$weight6weeks
diet_clean$diet <- as.factor(diet_clean$diet)
diet_clean$gender <- as.factor(diet_clean$gender)


diet_1 <- diet_clean %>% dplyr::filter(diet==1)
diet_2 <- diet_clean %>% dplyr::filter(diet==2)
diet_3 <- diet_clean %>% dplyr::filter(diet==3)

hist(diet_clean$preweight, breaks =10, col = "blue")
hist(diet_clean$weight6weeks, breaks =10, col = "blue")
hist(diet_clean$weight_lost, breaks =10, col = "blue")

qqnorm(diet_clean$preweight, col = "blue", lwd = 2)
qqline(diet_clean$preweight, col = "blue", lwd = 2)
ggqqplot(diet_clean$preweight)

qqnorm(diet_clean$weight6weeks, col = "blue", lwd = 2)
qqline(diet_clean$weight6weeks, col = "blue", lwd = 2)
ggqqplot(diet_clean$weight6weeks)
plot(diet_clean$age, diet_clean$preweight, xlab = "Preweight", ylab = "Weight6weeks", main = "Preweight vs Weight6weeks", col = "blue")
plot(diet_clean$age, diet_clean$weight6weeks, xlab = "Preweight", ylab = "Weight6weeks", main = "Preweight vs Weight6weeks", col = "blue")

plot(diet_clean$age, diet_clean$diet_difference, xlab = "Age", ylab = "Diet Difference", main = "Age vs Diet Difference", col = "blue")
plot(diet_clean$height, diet_clean$diet_difference, xlab = "Age", ylab = "Diet Difference", main = "Age vs Diet Difference", col = "blue")

boxplot_df <- melt(diet_clean, id.vars = setdiff(colnames(diet_clean), c("weight6weeks", "preweight")))
#boxplot_df$diet <- as.character(boxplot_df$diet)
#boxplot_df$gender <- as.character(boxplot_df$gender)
ggplot(boxplot_df, aes(x = diet, y = value, fill = variable )) + geom_boxplot()
ggplot(boxplot_df, aes(x = gender, y = value, fill = variable )) + geom_boxplot()


#TESTING ASSUMPTION OF NORMALITY
#VISUAL TEST
hist(diet_clean$weight_lost, breaks =10, col = "blue")
qqnorm(diet_clean$weight_lost, col = "blue", lwd = 2)
qqline(diet_clean$weight_lost, col = "blue", lwd = 2)
shapiro.test(diet_clean$weight_lost)

#DATA IS NORMAL
t.test(diet_clean$preweight, diet_clean$weight6weeks, paired = TRUE, alternative = "two.sided")


#QUESTION 2
#One Way ANOVA
#Checking homogeneous variance
#Levene's test

lm1 <- lm(weight_lost ~ diet, data = diet_clean)

leveneTest(weight_lost ~ diet, data = diet_clean)
#anova <- aov(weight_lost ~ diet, data = diet_clean)
ggqqplot(residuals(lm1))
shapiro.test(residuals(lm1))

anova(lm1)

#POST HOC TEST

#Tukey's HSD
TukeyHSD(aov(weight_lost ~ diet, data = diet_clean))
plot(aov(weight_lost ~ diet, data = diet_clean))

#QUESTION 3
#Two Way ANOVA

#Measuring interaction between diet and gender and weight loss

lm2 <- lm(weight_lost ~ diet * gender, data = diet_clean)
anova(lm2)
ggqqplot(residuals(lm2))
shapiro.test(residuals(lm2))

#Select only gender and diet columns
df <- diet_clean %>% dplyr::select(c("gender","diet")) %>% distinct()
predictions <- data.frame(predict(lm2, newdata =df))
colnames(predictions) <- "predicted_weight_lost"
df1 <- merge(df, predictions, by = "row.names")
df1 <- df1[,-1]



# ```{r, fig.show="hold", out.width = "50%"}
# qqnorm(diet_clean$preweight, col = "blue", lwd = 2, main = "Normal Q-Q plot of weight before diet", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
# qqline(diet_clean$preweight, col = "blue", lwd = 2, main = "Normal Q-Q plot of weight before diet", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
#
# qqnorm(diet_clean$weight6weeks, col = "blue", lwd = 2, main = "Normal Q-Q plot of weight after 6 weeks of dieting", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
# qqline(diet_clean$weight6weeks, col = "blue", lwd = 2, main = "Normal Q-Q plot of weight after 6 weeks of dieting", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
# ```