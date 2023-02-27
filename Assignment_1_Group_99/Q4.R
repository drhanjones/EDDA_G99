library(MASS)

npk

#Group by plot and Nitrogen for average yield

ggplot(npk %>% group_by(block, N) %>% summarise(mean_yield = mean(yield)), aes(x = block, y = mean_yield, fill = N)) + geom_bar(stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 0, hjust = 1)) + xlab("Plot") + ylab("Average Yield") + ggtitle("Average Yield by Plot and Nitrogen")


#2 way ANOVA with response variable yield and explanatory variables block and N

lm3 <- lm(yield ~ block * N, data = npk)
anova(lm3)


lm4 <- lm(yield ~ block * N + P + K, data = npk)
lm5 <- lm(yield ~ block * P + N + K, data = npk)
lm6 <- lm(yield ~ block * K + P + N, data = npk)
anova(lm4)
anova(lm5)
anova(lm6)