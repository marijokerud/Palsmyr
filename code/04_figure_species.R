library(ggplot2)
library(ggh4x)
library(wesanderson)

test<-wes_palette("Darjeeling1", n=5)
strip <- strip_themed(background_x = elem_list_rect(fill = test)) 

#FIELD
ggplot(speciesline, aes(x = year, y = field)) +
  geom_point() +
  stat_smooth(method="glm", color="black", se=TRUE,
              method.args = list(family=binomial(link = "logit"))) +
  facet_wrap2(~site, strip = strip) +
  labs(x = "Year", y= "Field layer") +
  theme_bw() 

#BOTTOM
ggplot(speciesline, aes(x = year, y = bottom)) +
  geom_point() +
  stat_smooth(method="glm", color="black", se=TRUE,
              method.args = list(family=binomial(link = "logit"))) +
  facet_wrap2(~site, strip = strip) +
  labs(x = "Year", y= "Bottom layer") +
  theme_bw() 

#SHRUB
ggplot(speciesline, aes(x = year, y = shrub)) +
  geom_point() +
  stat_smooth(method="glm", color="black", se=TRUE,
              method.args = list(family=binomial(link = "logit"))) +
  facet_wrap2(~site, strip = strip) +
  labs(x = "Year", y= "Shrub layer") +
  theme_bw() 

#save as 800x700  

theme(strip.background = element_rect(fill="test"))
theme(panel.grid.minor.x=element_blank(),    #Hide all the vertical gridlines
      panel.grid.major.x=element_blank(),    #panel.grid.minor.y=element_blank()  #Hide all the horizontal gridlines 
      panel.grid.major.y=element_blank()) 
