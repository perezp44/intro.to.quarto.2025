#- easystats: (https://easystats.github.io/easystats/)
#- An R Framework for Easy Statistical Modeling, Visualization, and Reporting
#- install.packages("easystats")

#- report: https://easystats.github.io/report/ ----------------
library(tidyverse)
library(easystats)

report(iris)

iris %>% select(-starts_with("Sepal")) %>% group_by(Species) %>%
  report() %>% summary()

report(t.test(mtcars$mpg ~ mtcars$am))

cor.test(iris$Sepal.Length, iris$Sepal.Width) %>%
  report() %>%  as.data.frame()

aov(Sepal.Length ~ Species, data = iris) %>% report()


model <- lm(Sepal.Length ~ Species, data = iris)
summary(model)
report(model)

model <- glm(vs ~ mpg * drat, data = mtcars, family = "binomial")
summary(model)
report(model)


library(lme4)
model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
summary(model)
report(model)


#- parameters: https://easystats.github.io/parameters/

model <- lm(Sepal.Width ~ Petal.Length * Species + Petal.Width, data = iris)
model_parameters(model)
model_parameters(model, standardize = "refit")


#- see: https://easystats.github.io/see/

results <- summary(correlation(iris))
plot(results, show_data = "points")


model <- lm(wt ~ am * cyl, data = mtcars)
summary(model)
plot(parameters(model))



model <- lm(mpg ~ factor(cyl) * wt, data = mtcars)
summary(model)
results <- fortify(model)

# step-3
ggplot(results) +
  geom_point(aes(x = wt, y = mpg, color = `factor(cyl)`)) +
  geom_line(aes(x = wt, y = .fitted, color = `factor(cyl)`))
