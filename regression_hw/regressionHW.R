library(datasets)
data(mtcars)

# does transmission type have an effect on mpg  

# exploratory
ggplot(data = mtcars, aes(x = factor(am), y = mpg, color = am)) + geom_point()

# means
mean(mtcars$mpg[mtcars$am == 0])
mean(mtcars$mpg[mtcars$am == 1])

# transmission type alone
model1 <- lm(data = mtcars, formula = mpg~am)
summary(model1)

# transmission and weight
model2 <- lm(data = mtcars, formula = mpg ~ am + wt)
summary(model2)

# weight alone
model3 <- lm(data = mtcars, formula = mpg ~ wt)

# residual plot
residual_df <- data.frame(row.names=names(model3$residuals), resid = model3$residuals)
residual_df$AM <- mtcars$am
ggplot(data = residual_df, aes(x=resid, fill = as.factor(AM))) + geom_histogram(binwidth=1.5)
# leverage
plot(hat(model.matrix(model2)))




