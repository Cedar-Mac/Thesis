require(mosaic)
Everything2014 <- read_excel("~/Desktop/OneDrive - Oregon State University/Stream Ecology/Invert other R code/2014 Everything.xlsx")


mod <- lm(Family ~ `Coll. Date` + Stream + Reach + Treatment, data = bugs1718)

pairs(~ age + weight + height + chest, data = bodyfat, pch = 19,
      col = "blue", lower.panel = NULL, main = "Body Fat example",
      labels = c("age (years)","weight (pounds)","height (inches)",
                 "chest circumference (cm)"))

cor(bodyfat[c("age","weight","height","chest")])

mod <- lm(pctfat ~ age + height + chest, data = bodyfat)
res <- residuals(mod)
pred <- fitted(mod)
plot(pred, res, pch = 18, cex = 1.5, col = "blue",
     main = "Residual Plot",
     xlab = "predicted values (%)",
     ylab = "residuals (%)", cex.axis = 1.4, cex.lab = 1.4)
abline(h=0)

plot(bodyfat$age, res, pch = 18, cex = 1.8, col = "darkgreen",
     main = "Residual Plot",
     xlab = "age (years)",
     ylab = "residuals (%)", cex.axis = 1.4, cex.lab = 1.4)
mtext("residuals vs age")
abline(h=0)

pairs(~ pctfat + age + weight + height + chest, data = bodyfat, pch = 19,
      col = "blue", lower.panel = NULL, main = "Body Fat example",
      labels = c("age (years)","weight (pounds)","height (inches)",
                 "chest circumference (cm)"))

qqnorm(res, pch = 17, cex = 1.4, cex.lab = 1.4, col = "blue",
       main = "Normal Probability Plot of Residuals")
qqline(res, lwd = 2)

summary(mod)

pairs(~ Age + Fat + Cholesterol, data = nutrition, pch = 19,
      col = "blue", lower.panel = NULL, main = "Body Fat example",
      labels = c("age (years)","weight (pounds)","height (inches)",
                 "chest circumference (cm)"))

all.mod <- lm(Calories ~ Age + Fat + Cholesterol, data = nutrition)
res <- residuals(mod)
pred <- fitted(mod)
plot(pred, res, pch = 18, cex = 1.5, col = "blue",
     main = "Residual Plot",
     xlab = "predicted values (%)",
     ylab = "residuals (%)", cex.axis = 1.4, cex.lab = 1.4)
abline(h=0)

summary(all.mod)

age.mod <- lm(Calories ~ Age, data = nutrition)
summary(age.mod)

fc.mod <- lm(Calories ~ Fat + Cholesterol, data = nutrition)
summary(fc.mod)
predict(all.mod, data.frame(Age = 50, Cholesterol = 350, Fat = 40))