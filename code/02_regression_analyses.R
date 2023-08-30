structurelinePSL
library(Rcurl)
library(lme4)
library(ggplot2)

structurelinePSL$pals = factor(structurelinePSL$pals)

summary(structurelinePSL$pals) #check ratio

m1 <- glmer(pals ~ year * site + (1 | line), 
          family = binomial(link = "logit"),
          data = structurelinePSL)
summary(m1)

m2 <- glmer(pals ~ year * site + (1 | line) + (1 | overdisp), 
            family = binomial(link = "logit"),
            data = structurelinePSL)
summary(m2)

AIC(logLik(m1))
AIC(logLik(m2)) # AIC is higher than m1, m2 generated a warning because the model is overfitted (overdisp variance is 0) – that is, the random effects structure is too complex to be supported by the data

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))