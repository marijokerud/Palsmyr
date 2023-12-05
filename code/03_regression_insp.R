

fit1.1 <- lm(NMDS1~ AAR * Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit1.1)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS1~ AAR+ Meter_from_ditch ,data = point.scores)
#abline(fit1.1)
plot(fit1.1, which = 1)
hist(fit1.1$residuals) # Histogram of residuals
plot(fit1.1, which = 2)


fit1.2 <- lm(NMDS1~ AAR + Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit1.2)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS1~ AAR+ Meter_from_ditch ,data = point.scores)
#abline(fit1.2)
plot(fit1.2, which = 1)
hist(fit1.2$residuals) # Histogram of residuals
plot(fit1.2, which = 2)

anova(fit1.1, fit1.2)

fit1.3 <- lm(NMDS1~ Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit1.3)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS1~ AAR+ Meter_from_ditch ,data = point.scores)
#abline(fit1.3)
plot(fit1.3, which = 1)
hist(fit1.3$residuals) # Histogram of residuals
plot(fit1.3, which = 2)

anova(fit1.2, fit1.3)


fit2.1 <- lm(NMDS2~ AAR+ Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit2.1)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS2~ AAR+ Meter_from_ditch, data = point.scores)
#abline(fit2.1)
plot(fit2.1, which = 1)
hist(fit2.1$residuals) # Histogram of residuals
plot(fit2.1, which = 2)


fit2.2 <- lm(NMDS2~ AAR*Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit2.2)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS2~ AAR*Meter_from_ditch ,data = point.scores)
#abline(fit2.2)
plot(fit2.2, which = 1)
hist(fit2.2$residuals) # Histogram of residuals
plot(fit2.2, which = 2)





### JOACHIM metode
fitxxxx2 <- lm(NMDS1~ Meter_from_ditch + I(Meter_from_ditch^(1/2)), data = point.scores)
summary(fitxxxx2)

fitxxxx3 <- lm(NMDS1~ Meter_from_ditch + I(Meter_from_ditch^2), data = point.scores)
summary(fitxxxx3)

par(mfrow = c(1,1))
plot(NMDS1~Meter_from_ditch ,data = point.scores)
points(0:50,predict(fitxxxx2,newdata=data.frame(Meter_from_ditch=(0:50))),type='l',col="blue")
points(0:50,predict(fitxxxx3,newdata=data.frame(Meter_from_ditch=(0:50))),type='l',col="purple")
