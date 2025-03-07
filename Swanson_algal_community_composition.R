######## Community composition PERMANOVA and NMDS analyses and figures for Swanson et al., 
### "Light color and nutrient availability interact to determine freshwater algal community diversity and composition" MS

library(vegan)
library(tidyverse)
library(readr)
library(scales)

#### genera PERMANOVA for day 54 communities, Table 2 in main manuscript
genera_data<-read.csv("comm_exp_genera2.csv") 
genera<-genera_data[,-c(1:8)]#### drops columns of non-numeric data
genera_dis<-vegdist(genera,method="bray",na.rm=TRUE)

adonis2(genera_dis~Light*Phosphorus,data=genera_data,by="terms") 

######### Class PERMANOVA for day 54 communities, Table S5 in supplement
class_counts<-read.csv("comm_exp_class.csv")
class_counts2<-class_counts[,-c(1:8)]
class_dis<-vegdist(class_counts2,method="bray",na.rm=TRUE) 

adonis2(class_dis~Light*Phosphorus,data=class_counts) 

######### NMDS for class level
mds<-metaMDS(class_dis,distance="bray",trymax=1000) # K=.100
mds_data <-as.data.frame(scores(mds))
mds_data$Treatment <-as.factor(class_counts$Treatment)
mds_data$Light <-as.factor(class_counts$Light)
mds_data$Phosphorus <-as.factor(class_counts$Phosphorus)

#### extract data to plot hulls
red_H_c_end <- mds_data[mds_data$Treatment == "Red_HP", ][chull(mds_data[mds_data$Treatment == "Red_HP", c("NMDS1", "NMDS2")]), ]  # hull values for red HP
red_L_c_end <- mds_data[mds_data$Treatment == "Red_LP", ][chull(mds_data[mds_data$Treatment == "Red_LP", c("NMDS1", "NMDS2")]), ]  # hull values for red LP
blue_H_c_end <- mds_data[mds_data$Treatment == "Blue_HP", ][chull(mds_data[mds_data$Treatment == "Blue_HP", c("NMDS1", "NMDS2")]), ]  # hull values for blue HP
blue_L_c_end <- mds_data[mds_data$Treatment == "Blue_LP", ][chull(mds_data[mds_data$Treatment == "Blue_LP", c("NMDS1", "NMDS2")]), ]  # hull values for blue LP
green_H_c_end <- mds_data[mds_data$Treatment == "Green_HP", ][chull(mds_data[mds_data$Treatment == "Green_HP", c("NMDS1", "NMDS2")]), ]  # hull values for green HP
green_L_c_end <- mds_data[mds_data$Treatment == "Green_LP", ][chull(mds_data[mds_data$Treatment == "Green_LP", c("NMDS1", "NMDS2")]), ]  # hull values for green LP
broad_H_c_end <- mds_data[mds_data$Treatment == "Broad_HP", ][chull(mds_data[mds_data$Treatment == "Broad_HP", c("NMDS1", "NMDS2")]), ]  # hull values for red HP
broad_L_c_end <- mds_data[mds_data$Treatment == "Broad_LP", ][chull(mds_data[mds_data$Treatment == "Broad_LP", c("NMDS1", "NMDS2")]), ]  # hull values for red LP

hull.data <- rbind(blue_H_c_end,blue_L_c_end,broad_H_c_end,broad_L_c_end,green_H_c_end,green_L_c_end,red_H_c_end,red_L_c_end)  #combine grp.a and grp.b

#### Class plot

dat_text1 <- data.frame(
  label = "K=0.100",
  Phosphorus= "Low",
  Light="Broad",
  Treatment="Broad_LP",
  x=2,
  y=-1)

######## plot for class level, day 54
ggplot(mds_data, aes(x = NMDS1, y = NMDS2,color=Light)) +
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Treatment),alpha=0.50) + # add the convex hulls
  geom_point(size=2.5)+
  facet_wrap(~Phosphorus,nrow=2,scales = "free_x",labeller = labeller(Phosphorus=c("High"="High Nutrient","Low"="Low Nutrient")))+
  theme_classic()+
  scale_x_continuous(limits=c(-2.5,2.5))+
  scale_color_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  scale_fill_manual(values = c("dodgerblue3","dodgerblue3","grey25","grey25","seagreen","seagreen","firebrick","firebrick"))+
  geom_text(data=dat_text1,
            mapping = aes(x=x,y=y,label = label))+
  guides(fill="none")+
  guides(color=guide_legend(title="Light Color",override.aes = list(fill=c("dodgerblue3","grey25","seagreen","firebrick"))))+
  labs(x="NMDS1",y="NMDS2",title = "Class Day 54")+
  theme(strip.text.x = element_text(size=14))+
  theme(strip.background = element_rect(linetype = "blank"))+
  theme(axis.title =element_text(size=16,))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(strip.text = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=16))+
  theme(strip.background = element_rect(fill="white",color="white"))+
  theme(strip.text = element_text(color="black",size = 14))+
  theme(plot.title = element_text(hjust = 0.5,size=16))

#ggsave("nmds_class_54.pdf",plot = last_plot(),device = pdf,width = 5,height = 5,dpi=320)


##### NMDS at genus level
mds2<-metaMDS(genera_dis,distance="bray",trymax=1000) #K=.148

mds_data2<-as.data.frame(mds2$points)
mds_data2$Treatment <-genera_data$Treatment
mds_data2$Light <-genera_data$Light
mds_data2$Phosphorus <-genera_data$Phosphorus

red_H_g_end <- mds_data2[mds_data2$Treatment == "Red_HP", ][chull(mds_data2[mds_data2$Treatment == "Red_HP", c("MDS1", "MDS2")]), ]  # hull values for red HP
red_L_g_end <- mds_data2[mds_data2$Treatment == "Red_LP", ][chull(mds_data2[mds_data2$Treatment == "Red_LP", c("MDS1", "MDS2")]), ]  # hull values for red LP
blue_H_g_end <- mds_data2[mds_data2$Treatment == "Blue_HP", ][chull(mds_data2[mds_data2$Treatment == "Blue_HP", c("MDS1", "MDS2")]), ]  # hull values for blue HP
blue_L_g_end <- mds_data2[mds_data2$Treatment == "Blue_LP", ][chull(mds_data2[mds_data2$Treatment == "Blue_LP", c("MDS1", "MDS2")]), ]  # hull values for blue LP
green_H_g_end <- mds_data2[mds_data2$Treatment == "Green_HP", ][chull(mds_data2[mds_data2$Treatment == "Green_HP", c("MDS1", "MDS2")]), ]  # hull values for green HP
green_L_g_end <- mds_data2[mds_data2$Treatment == "Green_LP", ][chull(mds_data2[mds_data2$Treatment == "Green_LP", c("MDS1", "MDS2")]), ]  # hull values for green LP
broad_H_g_end <- mds_data2[mds_data2$Treatment == "Broad_HP", ][chull(mds_data2[mds_data2$Treatment == "Broad_HP", c("MDS1", "MDS2")]), ]  # hull values for red HP
broad_L_g_end <- mds_data2[mds_data2$Treatment == "Broad_LP", ][chull(mds_data2[mds_data2$Treatment == "Broad_LP", c("MDS1", "MDS2")]), ]  # hull values for red LP

hull.data2 <- rbind(blue_H_g_end,blue_L_g_end,broad_H_g_end,broad_L_g_end,green_H_g_end,green_L_g_end,red_H_g_end,red_L_g_end)  #combine grp.a and grp.b

#### genus level plot at day 54, Figure 2 in main manuscript

dat_text2 <- data.frame(
  label = "K=0.148",
  Phosphorus= "Low",
  Light="Broad",
  Treatment="Broad_LP",
  x=2,
  y=-1)

ggplot(mds_data2, aes(x = MDS1, y = MDS2,color=Light)) +
  geom_polygon(data=hull.data2,aes(x=MDS1,y=MDS2,fill=Treatment),alpha=0.50) + # add the convex hulls
  geom_point(size=2.5)+
  facet_wrap(~Phosphorus,nrow=2,scales = "free_x",labeller = labeller(Phosphorus=c("High"="High Nutrient","Low"="Low Nutrient")))+
  theme_classic()+
  scale_x_continuous(limits=c(-2.5,2.5))+
  scale_color_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  scale_fill_manual(values = c("dodgerblue3","dodgerblue3","grey25","grey25","seagreen","seagreen","firebrick","firebrick"))+
  geom_text(data=dat_text2,
            mapping = aes(x=x,y=y,label = label))+
  guides(fill="none")+
  guides(color=guide_legend(title="Light Color",override.aes = list(fill=c("dodgerblue3","grey25","seagreen","firebrick"))))+
  labs(x="NMDS1",y="NMDS2",title = "Genus Day 54")+
  theme(strip.text.x = element_text(size=14))+
  theme(strip.background = element_rect(linetype = "blank"))+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(strip.text = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=16))+
  theme(strip.background = element_rect(fill="white",color="white"))+
  theme(strip.text = element_text(color="black",size = 16))+
  theme(plot.title = element_text(hjust = 0.5,size=16))

#ggsave("nmds_genus_54.pdf",plot = last_plot(),device = cairo_pdf(),width = 5,height = 5,dpi=320,units="in")


################# Community composition at Day 0 timepoint
###########

##### genera PERMANOVA
day0_gen<-read_csv("day0_genera.csv")
day0_gen2<-day0_gen[,-c(1:8)]#### drops columns of non-numeric data
genera_zero_dis<-vegdist(day0_gen2,method="bray",na.rm=TRUE)

adonis2(genera_zero_dis~Light*Phosphorus,data=day0_gen,by="terms") 

######### Class day 0 PERMANOVA
day0_class<-read.csv("day0_class.csv")
day0_class2<-day0_class[,-c(1:8)]
class_zero_dis<-vegdist(day0_class2,method="bray",na.rm=TRUE) 

adonis2(class_zero_dis~Light*Phosphorus,data=day0_class,by="terms") 

mds_cl_zero<-metaMDS(class_zero_dis,distance="bray",trymax=1000)
mds_cl_zero2 <-as.data.frame(mds_cl_zero$points)
mds_cl_zero2$Treatment <-day0_class$Treatment
mds_cl_zero2$Light <-day0_class$Light
mds_cl_zero2$Phosphorus <-day0_class$Phosphorus

red_H <- mds_cl_zero2[mds_cl_zero2$Treatment == "Red_HP", ][chull(mds_cl_zero2[mds_cl_zero2$Treatment == "Red_HP", c("MDS1", "MDS2")]), ]  # hull values for red HP
red_L <- mds_cl_zero2[mds_cl_zero2$Treatment == "Red_LP", ][chull(mds_cl_zero2[mds_cl_zero2$Treatment == "Red_LP", c("MDS1", "MDS2")]), ]  # hull values for red LP
blue_H <- mds_cl_zero2[mds_cl_zero2$Treatment == "Blue_HP", ][chull(mds_cl_zero2[mds_cl_zero2$Treatment == "Blue_HP", c("MDS1", "MDS2")]), ]  # hull values for blue HP
blue_L <- mds_cl_zero2[mds_cl_zero2$Treatment == "Blue_LP", ][chull(mds_cl_zero2[mds_cl_zero2$Treatment == "Blue_LP", c("MDS1", "MDS2")]), ]  # hull values for blue LP
green_H <- mds_cl_zero2[mds_cl_zero2$Treatment == "Green_HP", ][chull(mds_cl_zero2[mds_cl_zero2$Treatment == "Green_HP", c("MDS1", "MDS2")]), ]  # hull values for green HP
green_L <- mds_cl_zero2[mds_cl_zero2$Treatment == "Green_LP", ][chull(mds_cl_zero2[mds_cl_zero2$Treatment == "Green_LP", c("MDS1", "MDS2")]), ]  # hull values for green LP
broad_H <- mds_cl_zero2[mds_cl_zero2$Treatment == "Broad_HP", ][chull(mds_cl_zero2[mds_cl_zero2$Treatment == "Broad_HP", c("MDS1", "MDS2")]), ]  # hull values for red HP
broad_L <- mds_cl_zero2[mds_cl_zero2$Treatment == "Broad_LP", ][chull(mds_cl_zero2[mds_cl_zero2$Treatment == "Broad_LP", c("MDS1", "MDS2")]), ]  # hull values for red LP

hull.data3 <- rbind(blue_H,blue_L,broad_H,broad_L,green_H,green_L,red_H,red_L)

dat_text3 <- data.frame(
  label = "K=0.148",
  Phosphorus= "Low",
  Light="Broad",
  Treatment="Broad_LP",
  x=2,
  y=-1)
################## Class plot day 0
ggplot(mds_cl_zero2, aes(x = MDS1, y = MDS2,color=Light)) +
  geom_polygon(data=hull.data3,aes(x=MDS1,y=MDS2,fill=Treatment),alpha=0.50) + # add the convex hulls
  geom_point(size=2.5)+
  geom_polygon(data=hull.data3,aes(x=MDS1,y=MDS2,fill=Treatment),alpha=0.50) + # add the convex hulls
  facet_wrap(~Phosphorus,nrow=2,scales = "free_x",labeller = labeller(Phosphorus=c("High"="High Nutrient","Low"="Low Nutrient")))+
  theme_classic()+
  scale_x_continuous(limits=c(-2.5,2.5))+
  scale_color_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  scale_fill_manual(values = c("dodgerblue3","dodgerblue3","grey25","grey25","seagreen","seagreen","firebrick","firebrick"))+
  labs(x="NMDS1",y="NMDS2",title = "Class Day 0")+
  guides(fill="none")+
  guides(color=guide_legend(title="Light Color",override.aes = list(fill=c("dodgerblue3","grey25","seagreen","firebrick"))))+
  theme(strip.text.x = element_text( size=14))+
  theme(strip.background = element_rect(linetype = "blank"))+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=16))+
  theme(strip.text = element_text(size=16))+
  theme(strip.background = element_rect(fill="white",color="white"))+
  theme(strip.text = element_text(color="black",size = 14))+
  geom_text(data=dat_text3,
            mapping = aes(x=x,y=y,label = label))+
  theme(plot.title = element_text(hjust = 0.5,size=16))

#ggsave("nmds_class_0.pdf",plot = last_plot(),device = pdf,width = 5,height = 5,dpi=320)


##### NMDS at genus level, day 0, Figure 2 in main MS
mds_gen_zero<-metaMDS(genera_zero_dis,distance="bray",trymax=1000)

mds_gen_zero2<-as.data.frame(mds_gen_zero$points)
mds_gen_zero2$Treatment <-day0_gen$Treatment
mds_gen_zero2$Light <-day0_gen$Light
mds_gen_zero2$Phosphorus <-day0_gen$Phosphorus

red_H_g <- mds_gen_zero2[mds_gen_zero2$Treatment == "Red_HP", ][chull(mds_gen_zero2[mds_gen_zero2$Treatment == "Red_HP", c("MDS1", "MDS2")]), ]  # hull values for red HP
red_L_g <- mds_gen_zero2[mds_gen_zero2$Treatment == "Red_LP", ][chull(mds_gen_zero2[mds_gen_zero2$Treatment == "Red_LP", c("MDS1", "MDS2")]), ]  # hull values for red LP
blue_H_g <- mds_gen_zero2[mds_gen_zero2$Treatment == "Blue_HP", ][chull(mds_gen_zero2[mds_gen_zero2$Treatment == "Blue_HP", c("MDS1", "MDS2")]), ]  # hull values for blue HP
blue_L_g <- mds_gen_zero2[mds_gen_zero2$Treatment == "Blue_LP", ][chull(mds_gen_zero2[mds_gen_zero2$Treatment == "Blue_LP", c("MDS1", "MDS2")]), ]  # hull values for blue LP
green_H_g <- mds_gen_zero2[mds_gen_zero2$Treatment == "Green_HP", ][chull(mds_gen_zero2[mds_gen_zero2$Treatment == "Green_HP", c("MDS1", "MDS2")]), ]  # hull values for green HP
green_L_g <- mds_gen_zero2[mds_gen_zero2$Treatment == "Green_LP", ][chull(mds_gen_zero2[mds_gen_zero2$Treatment == "Green_LP", c("MDS1", "MDS2")]), ]  # hull values for green LP
broad_H_g <- mds_gen_zero2[mds_gen_zero2$Treatment == "Broad_HP", ][chull(mds_gen_zero2[mds_gen_zero2$Treatment == "Broad_HP", c("MDS1", "MDS2")]), ]  # hull values for red HP
broad_L_g <- mds_gen_zero2[mds_gen_zero2$Treatment == "Broad_LP", ][chull(mds_gen_zero2[mds_gen_zero2$Treatment == "Broad_LP", c("MDS1", "MDS2")]), ]  # hull values for red LP

hull.data4 <- rbind(blue_H_g,blue_L_g,broad_H_g,broad_L_g,green_H_g,green_L_g,red_H_g,red_L_g)  #combine grp.a and grp.b

dat_text4 <- data.frame(
  label = "K=0.144",
  Phosphorus= "Low",
  Light="Broad",
  Treatment="Broad_LP",
  x=2,
  y=-1)

#### genus level plot, day 0
ggplot(mds_gen_zero2, aes(x = MDS1, y = MDS2,color=Light)) +
  geom_point(size=2.5)+
  geom_polygon(data=hull.data4,aes(x=MDS1,y=MDS2,fill=Treatment),alpha=0.50) + # add the convex hulls
  facet_wrap(~Phosphorus,nrow=2,scales = "free_x",labeller = labeller(Phosphorus=c("High"="High Nutrient","Low"="Low Nutrient")))+
  theme_classic()+
  scale_x_continuous(limits=c(-2.5,2.5))+
  scale_color_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  scale_fill_manual(values = c("dodgerblue3","dodgerblue3","grey25","grey25","seagreen","seagreen","firebrick","firebrick"))+
  labs(x="NMDS1",y="NMDS2",title = "Genus Day 0")+
  guides(fill="none")+
  guides(color=guide_legend(title="Light Color",override.aes = list(fill=c("dodgerblue3","grey25","seagreen","firebrick"))))+
  theme(strip.text.x = element_text( size=14))+
  theme(strip.background = element_rect(linetype = "blank"))+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(strip.text = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=16))+
  theme(strip.background = element_rect(fill="white",color="white"))+
  theme(strip.text = element_text(color="black",size = 14))+
  geom_text(data=dat_text4,
            mapping = aes(x=x,y=y,label = label))+
  theme(plot.title = element_text(hjust = 0.5,size=16))

#ggsave("nmds_genus_0.pdf",plot = last_plot(),device = pdf,width = 5,height = 5,dpi=320,units="in")
