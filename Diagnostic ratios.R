library(ggplot2)
library(stringi)
library(stringr)
data<-read.csv("E:/Results/PAH soil/Cross plot.csv")
data
Pre<-data[1:6,]
Burn<-data[7:12,]
Post<-data[13:18,]

label1<- " Mixed pyrogenic sources"
labelx<-stringr::str_wrap(label1, 15)
wrapper <- function(x, ...) paste(stri_wrap(x, ...), collapse = "\n")

ggplot(data, aes(x=FLT..FLT.PYR., y = PHEN..PHEN.ANT.))+
  geom_point(aes(color=factor(Phase),shape = factor(Phase)))+
  xlim(0,1.0)+
  ylim(0,1.0)

ggplot(data = data, aes(x=FLT..FLT.PYR., y = PHEN..PHEN.ANT.))+
  geom_rect(aes(xmin = .5,xmax = 1.0,ymin = .5,ymax = 1.0),fill = "yellow",alpha = 0.01)+
  geom_rect(aes(xmin = .4,xmax = 0.5,ymin = .5,ymax = 1.0),fill = "green",alpha = 0.05)+
  theme_bw()+
  geom_point(aes(shape = Phase, color=Phase), size = 3)+
  xlim(0,1.0)+
  ylim(0,1.0)+
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 0.4, linetype = "dashed", color = "black")+
  ylab ("Phen/(Phen+Ant)")+
  xlab("Flt/(Flt+Pyr)")+
  geom_segment(aes(x=0.51, xend = 1.0, y = 0.48, yend = 0.48), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x= 1.0, xend = 0.51, y = 0.48, yend = 0.48), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x=0.41, xend = .49, y = 0.48, yend = 0.48), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x= 0.49, xend = 0.41, y = 0.48, yend = 0.48), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x=0.0, xend = 0.39, y = 0.48, yend = 0.48), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x= 0.39, xend = 0.00, y = 0.48, yend = 0.48), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x=0.375, xend = .375, y = 0.51, yend = 0.99), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x= 0.375, xend = 0.375, y = 0.99, yend = 0.51), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x=0.375, xend = .375, y = 0.00, yend = 0.49), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_segment(aes(x= 0.375, xend = 0.375, y = 0.49, yend = 0.00), 
               arrow = arrow(angle = 30, length = unit(0.01, "npc"), type = "closed"))+
  geom_text(x=0.75, y=.55, label="Biomass combustion", size = 3)+
  geom_text(x=0.18, y=.55, label="Mixed sources (pyrogenic + petrogenic)", size = 3)+
  geom_text(x =0.45, y = 0.55, label = labelx, size = 3)+
  geom_text(x=0.35, y=.24, label="Mixed pyrogenic sources", size = 3, angle = 90)+
  geom_text(x=0.35, y=.72, label="Biomass combustion", size = 3, angle = 90)+
  theme(axis.text = element_text(face="bold"))+
  theme(axis.title.x = element_text(face="bold"))+
  theme(axis.title.y = element_text(face="bold"))
  
  
  


  # annotate("text", x=0.401, y=0.52, label=wrapper("Mixed pyrogenic sources"))

  
ggplot(Burn, aes(x=FLT..FLT.PYR., y = PHEN..PHEN.ANT.))+
  geom_point(shape = 15, color="red")+
  xlim(0,1.0)+
  ylim(0,1.0)

ggplot(Post, aes(x=FLT..FLT.PYR., y = PHEN..PHEN.ANT.))+
  geom_point(aes(color="blue"))+
  xlim(0,1.0)+
  ylim(0,1.0)
