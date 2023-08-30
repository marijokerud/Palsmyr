library(ggplot2)
library(ggrepel)
#library(wesanderson)

#KALDVASSMYRA
### POLYNOMIAL REGRESSION PLOT
ggplot(point.scores, aes(x= Meter_from_ditch, y=NMDS1)) +
  geom_point(shape=1, color= "black", size=2.2) +
  geom_smooth(method='lm', formula= y~x+sqrt(x), se=FALSE, col="black", lwd=1.2) +
  geom_hline(aes(yintercept = -0.2830547), linetype = "dotted", col = "#810f7c") +
  labs(x = "Meters from ditch", y= "NMDS1 scores") +
  theme_bw() +
  #annotate(geom = "rect", xmin= 0, xmax= 50, ymin= -0.69642, ymax= 0.1303105, fill="#810f7c", alpha=0.3) +
  annotate(geom = "rect", xmin= 0, xmax= 50, ymin= -0.615967, ymax= 0.0498576, fill="#810f7c", alpha=0.3) +
  #geom_rect(xmin= 0, xmax= 50, ymin= -0.69642, ymax= 0.1303105, fill= "purple") +
  #geom_tile(aes(y= -0.2830547, x= 25, width=50, height= 0.8267306, fill="red")) +
  #scale_fill_gradient2(low = "white", mid = "purple", high = "white", midpoint = -0.2830547) 
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        #panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank()) 

#save as 575x650

#col= "810f7c" og "8856a7"

myfill <- c("#f0f9e8", "#ccebc5", "#a8ddb5", "#7bccc4", "#43a2ca", "#0868ac", "black")
mycolors <- c("black", "black", "black", "black", "black", "black", "black")
myshapes <- c(21, 22, 24)

### NMDS SITE AND SPECIES PLOT
ggplot(data=all.point.scores, aes(x=NMDS1, y=NMDS2)) + 
  geom_point(data=all.point.scores, aes(x=NMDS1, y=NMDS2, fill = as.factor(Meter_from_ditch), color = as.factor(Meter_from_ditch), shape= AAR2),  size = 4) + # 
  labs(x = "NMDS1 scores", y= "NMDS2 scores", fill = "Distance from ditch", color = "Distance from ditch", shape = "Year") + #,
  scale_color_manual(values=myfill) +
  scale_fill_manual(values=myfill) +
  scale_shape_manual(values = c(21,22,24), labels=c("Before","Two years after", "Five years after"),name="Restoration") +
  theme_bw() +
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank()) +
  geom_text_repel(aes(label=linje), size=2.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 5)) +  #add smaller text
  geom_text_repel(data= species.scores, aes(x=NMDS1, y=NMDS2, label=species), size=3.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 15))

##geom_polygon(data=all.point.scores, aes(x = NMDS1, y = NMDS2, fill= linje, group=linje), alpha = 0.30) +

#save as 650x600 // 800x700

#HILDREMSVATNET

myfill <- c("#f0f9e8", "#ccebc5", "#43a2ca", "#0868ac") # "#a8ddb5", "#7bccc4",
mycolors <- c("black", "black", "black", "black")

### NMDS SITE AND SPECIES PLOT
ggplot(data=all.point.scores, aes(x=NMDS1, y=NMDS2)) + 
  geom_point(data=all.point.scores, aes(x=NMDS1, y=NMDS2, color = linje, fill = linje, shape = AAR2), size = 4) +  
  labs(x = "NMDS1 scores", y= "NMDS2 scores") + 
  scale_color_manual(values=mycolors, name="Transect") +
  scale_fill_manual(values=myfill, name="Transect") +
  scale_shape_manual(values = c(21,22), labels=c("Before","Two years after"), name="Restoration") +
  theme_bw() +
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank()) +
  geom_text_repel(aes(label=linje), size=2.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 5)) +  #add smaller text
  geom_text_repel(data= species.scores, aes(x=NMDS1, y=NMDS2, label=species), size=3.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 15))

##geom_polygon(data=all.point.scores, aes(x = NMDS1, y = NMDS2, fill= linje, group=linje), alpha = 0.30) +

#save as 650x600 // 800x700