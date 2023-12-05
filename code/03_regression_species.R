speciesline
library(lme4)

#PALS
#speciesline$pals = factor(speciesline$pals)
#summary(speciesline$pals) #check ratio

#NULL MODEL
glmer(field ~ 1 + (1|line), data = speciesline, family = binomial(link = "logit")) 

#FIELD
m1 <- glmer(field ~ year * site, #plotted lines show no interaction, slope in same direction
            family = binomial(link = "logit"),
            data = speciesline)
summary(m1) 


m1 <- glmer(field ~ year * site + (1 | line), #plotted lines show no interaction, slope in same direction
          family = binomial(link = "logit"),
          data = speciesline)
summary(m1) 

m2 <- glmer(field ~ year + site + (1 | line), 
            family = binomial(link = "logit"),
            data = speciesline)
summary(m2)

m3 <- glmer(field ~ year + site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = speciesline)
summary(m3)

m4 <- glmer(field ~ year * site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = speciesline)
summary(m4)

AIC(logLik(m1))
AIC(logLik(m2))
AIC(logLik(m3))
AIC(logLik(m4)) # M4 generated a warning because the model is overfitted (overdisp variance is 0) – that is, the random effects structure is too complex to be supported by the data


#BOTTOM
#speciesline$bottom = factor(speciesline$bottom)
#summary(speciesline$bottom) #check ratio

m1 <- glmer(bottom ~ year * site + (1|line), #plotted lines show no interaction, slope in same direction
            family = binomial(link = "logit"),
            data = speciesline,
            control=glmerControl(optimizer="bobyqa")) #got following error message: optimizer (Nelder_Mead) convergence code: 0 (OK)
summary(m1)                                           #Model failed to converge with max|grad| = 0.0027761 (tol = 0.002, component 1)
                                                      #Works with bobyqa

m2 <- glmer(bottom ~ year + site + (1|line), 
            family = binomial(link = "logit"),
            data = speciesline)
summary(m2)


m3 <- glmer(bottom ~ year + site + (1|line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = speciesline)
summary(m3)

m4 <- glmer(bottom ~ year * site + (1|line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = speciesline,
            control=glmerControl(optimizer="bobyqa"))
summary(m4)

AIC(logLik(m1))
AIC(logLik(m2))
AIC(logLik(m3))
AIC(logLik(m4))


#SHRUB
#speciesline$shrub = factor(speciesline$shrub)
#summary(speciesline$shrub) #check ratio

m1 <- glmer(shrub ~ year * site + (1 | line), #plotted lines show no interaction, slope in same direction
            family = binomial(link = "logit"),
            data = speciesline,
            control=glmerControl(optimizer="bobyqa"))#got following error message: optimizer (Nelder_Mead) convergence code: 0 (OK)
summary(m1)                                           #Model failed to converge with max|grad| = 0.0027761 (tol = 0.002, component 1)
                                                      #Works with bobyqa

m2 <- glmer(shrub ~ year + site + (1 | line), 
            family = binomial(link = "logit"),
            data = speciesline)
summary(m2)

m3 <- glmer(shrub ~ year + site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = speciesline)
summary(m3)

m4 <- glmer(shrub ~ year * site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = speciesline)
summary(m4)

AIC(logLik(m1))
AIC(logLik(m2))
AIC(logLik(m3))
AIC(logLik(m4))