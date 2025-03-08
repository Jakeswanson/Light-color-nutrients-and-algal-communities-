######## Rank abundance curves for Swanson et al., 
### "Light color and nutirent availability interact to determine freshwater algal community diversity and composition" MS
### Code generates an individual rank abundance curve and figure for each treatment,
### all figures then grouped together to form Figure S2 in the supplement

library(BiodiversityR)
library(tidyverse)
library(scales)
library(ggtext)

####### with all genera data
rac_gen<-read.csv("comm_exp_genera2.csv")
rac_gen$Treatment<-as.factor(rac_gen$Treatment)
rac_gen<-rac_gen %>% mutate(across(c(9:69), ~replace_na(.x, 0)))
         
rankgen<-as.data.frame(rac_gen[,-c(1:8)])

rac2<- rankabuncomp(rankgen, y=rac_gen, factor="Treatment",
                    return.data=TRUE, specnames=c(1:10), legend=FALSE)
##### above code generates rank abundance info for every species in a treatment
#### only the top ten were included for use in the MS, once rac2 is read in below it yields an edited csv that only shows the top ten taxa for each treatment

#write_csv(rac2,"rac_gen.csv")### creating a file with the rank abundances of all taxa for each treatment
rac2<-read.csv("rac_gen.csv") ### edited in excel to only have the top ten most abundant taxa by treatment

###### Pull out the taxa for each treatment, create proportions and upper and lower CIs, and ordered the species for figure labeling
bh_rac<-rac2[1:10,]
bh_rac$proportion<-bh_rac$proportion/100
bh_rac$plower<-bh_rac$plower/100
bh_rac$pupper<-bh_rac$pupper/100
bh_rac$Total_prop<-bh_rac$abundance/421857.14 ### total abundance across all counts of a treatment
bh_rac$species <- ordered(bh_rac$species, levels=c("Non_flagellated_green","Desmodesmus_sp","Crypto_sp1","Unknown_green","Crypto_sp2","Didyomogenes_sp","M_contortum","Flagellated_greens","Dinoflagellates","Coccoid_cyano"))

bl_rac<-rac2[11:20,]
bl_rac$proportion<-bl_rac$proportion/100
bl_rac$plower<-bl_rac$plower/100
bl_rac$pupper<-bl_rac$pupper/100
bl_rac$Total_prop<-bl_rac$abundance/286500
bl_rac$species <- ordered(bl_rac$species, levels=c("Non_flagellated_green",	"Aphanocapsa_sp",	"Unknown_green","Closterium_aciculare",	"Golenkinia_sp","M_contortum","Pseudanabaena_sp","Coccoid_cyano","Flagellated_greens","Dinoflagellates"))

brh_rac<-rac2[21:30,]
brh_rac$proportion<-brh_rac$proportion/100
brh_rac$plower<-brh_rac$plower/100
brh_rac$pupper<-brh_rac$pupper/100
brh_rac$Total_prop<-brh_rac$abundance/2695000
brh_rac$species <- ordered(brh_rac$species, levels=c("Jaaginema_sp","Non_flagellated_green","Pseudanabaena_sp",	"Anabaena_sp","Aphanocapsa_sp","Flagellated_greens","Coccoid_cyano","Merismopedia_sp","Golenkinia_sp","Tetrastrum_sp"))

brl_rac<-rac2[31:40,]
brl_rac$proportion<-brl_rac$proportion/100
brl_rac$plower<-brl_rac$plower/100
brl_rac$pupper<-brl_rac$pupper/100
brl_rac$Total_prop<-brl_rac$abundance/1455571.43
brl_rac$species <- ordered(brl_rac$species, levels=c("Aphanocapsa_sp","Jaaginema_sp","Filamentous_cyano","Coccoid_cyano", "Aphanizomenon_sp","Non_flagellated_green","Pseudanabaena_sp","Golenkinia_sp","Closterium_aciculare","Flagellated_greens"))

gh_rac<-rac2[41:50,]
gh_rac$proportion<-gh_rac$proportion/100
gh_rac$plower<-gh_rac$plower/100
gh_rac$pupper<-gh_rac$pupper/100
gh_rac$Total_prop<-gh_rac$abundance/1766535.71
gh_rac$species <- ordered(gh_rac$species, levels=c("Aphanocapsa_sp","Non_flagellated_green","Jaaginema_sp","Anabaena_sp","Desmodesmus_sp","Dictyosphaerium_sp","M_contortum","Selenastrum_sp","Scenedesmus_sp","Flagellated_greens"))

gl_rac<-rac2[51:60,]
gl_rac$proportion<-gl_rac$proportion/100
gl_rac$plower<-gl_rac$plower/100
gl_rac$pupper<-gl_rac$pupper/100
gl_rac$Total_prop<-gl_rac$abundance/653571.43
gl_rac$species <- ordered(gl_rac$species, levels=c("Aphanocapsa_sp","Non_flagellated_green","Mougeotia_sp","Anabaena_sp","Jaaginema_sp","Pseudanabaena_sp","M_contortum",	"Flagellated_greens","Golenkinia_sp","Dinoflagellates"))

rh_rac<-rac2[61:70,]
rh_rac$proportion<-rh_rac$proportion/100
rh_rac$plower<-rh_rac$plower/100
rh_rac$pupper<-rh_rac$pupper/100
rh_rac$Total_prop<-rh_rac$abundance/1063071.43
rh_rac$species <- ordered(rh_rac$species, levels=c("Pseudanabaena_sp","Aphanocapsa_sp","Non_flagellated_green","Desmodesmus_sp","Unknown_green","Merismopedia_sp","Jaaginema_sp","Didyomogenes_sp","Flagellated_greens","M_contortum"))

rl_rac<-rac2[71:80,]
rl_rac$proportion<-rl_rac$proportion/100
rl_rac$plower<-rl_rac$plower/100
rl_rac$pupper<-rl_rac$pupper/100
rl_rac$Total_prop<-rl_rac$abundance/1377428.57
rl_rac$species <- ordered(rl_rac$species, levels=c("Aphanocapsa_sp","Jaaginema_sp","Unknown_green","Pseudanabaena_sp","Aphanizomenon_sp","Non_flagellated_green",	"Crypto_sp2","Golenkinia_sp","Closterium_aciculare","Cyanodictyon_sp"))

#### Rank abundance curves for each treatment, each treatment has its own figure that were combined to create Figure S2
####### Blue HP
ggplot(bh_rac, aes(x = species , y =proportion))+
  geom_line(linewidth=1.5,group=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=plower,ymax=pupper),linewidth=1)+
  guides(x=guide_axis(angle=60))+
  scale_y_continuous(labels = label_comma())+
  labs(y="Proportion of total biomass",title="Blue High Nutrients")+
  annotate("text",x=7,y=.55,label="421,857 cells/mL",size=6)+
  theme_classic()+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(title = element_text(size = 16))

#ggsave("blue_high.pdf",plot = last_plot(),device = cairo_pdf,width = 8,height = 6)

####Blue LP
ggplot(bl_rac, aes(x = species , y =proportion))+
  geom_line(linewidth=1.5,group=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=plower,ymax=pupper),linewidth=1)+
  guides(x=guide_axis(angle=60))+
  scale_y_continuous(labels = label_comma())+
  labs(y="Proportion of total biomass",title="Blue Low Nutrients")+
  annotate("text",x=7,y=.55,label="286,500 cells/mL",size=6)+
  theme_classic()+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(title = element_text(size = 16))

#ggsave("blue_low.pdf",plot = last_plot(),device = cairo_pdf,width = 8,height = 6)

####Green HP
ggplot(gh_rac, aes(x = species , y =proportion))+
  geom_line(linewidth=1.5,group=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=plower,ymax=pupper),linewidth=1)+
  guides(x=guide_axis(angle=60))+
  scale_y_continuous(labels = label_comma())+
  labs(y="Proportion of total biomass",title="Green High Nutrients")+
  annotate("text",x=7,y=.8,label="1,766,535 cells/mL",size=6)+
  theme_classic()+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(title = element_text(size = 16))

#ggsave("green_high.pdf",plot = last_plot(),device = cairo_pdf,width = 8,height = 6)

####Green LP
ggplot(gl_rac, aes(x = species , y =proportion))+
  geom_line(linewidth=1.5,group=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=plower,ymax=pupper),linewidth=1)+
  guides(x=guide_axis(angle=60))+
  scale_y_continuous(labels = label_comma())+
  labs(y="Proportion of total biomass",title="Green Low Nutrients")+
  annotate("text",x=7,y=.65,label="653,571 cells/mL",size=6)+
  theme_classic()+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(title = element_text(size = 16))

#ggsave("green_low.pdf",plot = last_plot(),device = cairo_pdf,width = 8,height = 6)


####Red HP
ggplot(rh_rac, aes(x = species , y =Total_prop))+
  geom_line(linewidth=1.5,group=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=plower,ymax=pupper),linewidth=1)+
  guides(x=guide_axis(angle=60))+
  scale_y_continuous(labels = label_comma())+
  labs(y="Proportion of total biomass",title="Red High Nutrients")+
  annotate("text",x=7,y=.4,label="1,063,071 cells/mL",size=6)+
  theme_classic()+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(title = element_text(size = 16))

#ggsave("red_high.pdf",plot = last_plot(),device = cairo_pdf,width = 8,height = 6)


####Red LP

ggplot(rl_rac, aes(x = species , y =Total_prop))+
  geom_line(linewidth=1.5,group=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=plower,ymax=pupper),linewidth=1)+
  guides(x=guide_axis(angle=60))+
  scale_y_continuous(labels = label_comma())+
  labs(y="Proportion of total biomass",title="Red Low Nutrients")+
  annotate("text",x=7,y=.55,label="1,377,428 cells/mL",size=6)+
  theme_classic()+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(title = element_text(size = 16))

#ggsave("red_low.pdf",plot = last_plot(),device = cairo_pdf,width = 8,height = 6)


####Broad HP
ggplot(brh_rac, aes(x = species , y =Total_prop))+
  geom_line(linewidth=1.5,group=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=plower,ymax=pupper),linewidth=1)+
  guides(x=guide_axis(angle=60))+
  scale_y_continuous(labels = label_comma())+
  labs(y="Proportion of total biomass",title="Broad High Nutrients")+
  annotate("text",x=7,y=.7,label="2,695,000 cells/mL",size=6)+
  theme_classic()+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(title = element_text(size = 16))

#ggsave("broad_high.pdf",plot = last_plot(),device = cairo_pdf,width = 8,height = 6)


####Broad LP
ggplot(brl_rac, aes(x = species , y =Total_prop))+
  geom_line(linewidth=1.5,group=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=plower,ymax=pupper),linewidth=1)+
  guides(x=guide_axis(angle=60))+
  scale_y_continuous(labels = label_comma())+
  labs(y="Proportion of total biomass",title="Broad Low Nutrients")+
  annotate("text",x=7,y=.7,label="1,455,571 cells/mL",size=6)+
  theme_classic()+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(title = element_text(size = 16))

#ggsave("broad_low.pdf",plot = last_plot(),device = cairo_pdf,width = 8,height = 6)



