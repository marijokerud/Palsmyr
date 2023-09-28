structurelinePSL
library(lme4)

#PALS
#structurelinePSL$pals = factor(structurelinePSL$pals)
#summary(structurelinePSL$pals) #check ratio

m1 <- glmer(pals ~ year * site + (1 | line), #plotted lines show no interaction, slope in same direction
          family = binomial(link = "logit"),
          data = structurelinePSL)
summary(m1) 

m2 <- glmer(pals ~ year + site + (1 | line), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m2)

m3 <- glmer(pals ~ year + site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m3)

m4 <- glmer(pals ~ year * site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m4)

AIC(logLik(m1))
AIC(logLik(m2))
AIC(logLik(m3))
AIC(logLik(m4)) # AIC is higher than m1, m4 generated a warning because the model is overfitted (overdisp variance is 0) – that is, the random effects structure is too complex to be supported by the data


#MYR
#structurelinePSL$myr = factor(structurelinePSL$myr)
#summary(structurelinePSL$myr) #check ratio

m1 <- glmer(myr ~ year * site + (1|line), #plotted lines show no interaction, slope in same direction
            family = binomial(link = "logit"),
            data = structurelinePSL,
            control=glmerControl(optimizer="bobyqa")) #got following error message: optimizer (Nelder_Mead) convergence code: 0 (OK)
summary(m1)                                           #Model failed to converge with max|grad| = 0.0027761 (tol = 0.002, component 1)
                                                      #Works with bobyqa

m2 <- glmer(myr ~ year + site + (1|line), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m2)


m3 <- glmer(myr ~ year + site + (1|line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m3)

m4 <- glmer(myr ~ year * site + (1|line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = structurelinePSL,
            control=glmerControl(optimizer="bobyqa"))
summary(m4)

AIC(logLik(m1))
AIC(logLik(m2))
AIC(logLik(m3))
AIC(logLik(m4))


#DAM
#structurelinePSL$dam = factor(structurelinePSL$dam)
#summary(structurelinePSL$dam) #check ratio

m1 <- glmer(dam ~ year * site + (1 | line), #plotted lines show no interaction, slope in same direction
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m1) 

m2 <- glmer(dam ~ year + site + (1 | line), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m2)

m3 <- glmer(dam ~ year + site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m3)

m4 <- glmer(dam ~ year * site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m4)

AIC(logLik(m1))
AIC(logLik(m2))
AIC(logLik(m3))
AIC(logLik(m4))