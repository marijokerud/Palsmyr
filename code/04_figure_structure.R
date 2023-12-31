library(ggplot2)
library(ggh4x)
library(wesanderson)

test<-wes_palette("Darjeeling1", n=5)
strip <- strip_themed(background_x = elem_list_rect(fill = test)) 

#PALS
ggplot(structurelinePSL, aes(x = year, y = pals)) +
  geom_point() +
  stat_smooth(method="glm", color="black", se=TRUE,
              method.args = list(family=binomial(link = "logit"))) +
  facet_wrap2(~site, strip = strip) +
  labs(x = "Year", y= "Pals") +
  theme_bw() 

#MYR
ggplot(structurelinePSL, aes(x = year, y = myr)) +
  geom_point() +
  stat_smooth(method="glm", color="black", se=TRUE,
              method.args = list(family=binomial(link = "logit"))) +
  facet_wrap2(~site, strip = strip) +
  labs(x = "Year", y= "Mire") +
  theme_bw() 

#DAM
ggplot(structurelinePSL, aes(x = year, y = dam)) +
  geom_point() +
  stat_smooth(method="glm", color="black", se=TRUE,
              method.args = list(family=binomial(link = "logit"))) +
  facet_wrap2(~site, strip = strip) +
  labs(x = "Year", y= "Dam") +
  theme_bw() 

#save as 800x700  

theme(strip.background = element_rect(fill="test"))
theme(panel.grid.minor.x=element_blank(),    #Hide all the vertical gridlines
      panel.grid.major.x=element_blank(),    #panel.grid.minor.y=element_blank()  #Hide all the horizontal gridlines 
      panel.grid.major.y=element_blank()) 
