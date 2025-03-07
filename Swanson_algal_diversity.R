######## Diversity indices calculations, analyses, and figures for Swanson et al., 
### "Light color and nutrient availability interact to determine freshwater algal community diversity and composition" MS
library(tidyverse)
library(BiodiversityR)
library(lme4)
library(lmerTest)
library(MuMIn)
library(car)
library(rstatix)
library(ggpubr)

#### Diversity indices for class
diversity_data<-read.csv("comm_exp_class.csv")
diversity_data2<-diversity_data[,-c(1:8)]

test<-diversity_data%>%group_by(Treatment)%>%summarise(across(c(8:15),sum))
test$Total<-rowSums(test[,c(2:9)])

div_ind<-as_tibble(diversity_data[,c(1:3)])

swd<-as_tibble(diversity(diversity_data2,index="shannon"))#### shannon
sid<-as_tibble(diversity(diversity_data2,index="simpson"))#### simpson
isd<-as_tibble(diversity(diversity_data2,index="invsimpson"))#### inverse simpson

div_ind<-div_ind %>% bind_cols(swd,sid,isd)
reps<-diversity_data%>% dplyr::select(Micro_Rep,Count_Rep)
div_ind<-bind_cols(div_ind, reps)
names(div_ind)<-c("Light","Phosphorus","Treatment","Shannon","Simpson","Inverse_Simpson","Micro_Rep","Count_Rep")

#### Diversity indices for genus
gen_data<-read.csv("comm_exp_genera2.csv")
gen_data2<-gen_data[,-c(1:8)]

gen_ind<-as_tibble(gen_data[,c(1:3)])

swd<-as_tibble(diversity(gen_data2,index="shannon"))#### shannon
sid<-as_tibble(diversity(gen_data2,index="simpson"))#### simpson
isd<-as_tibble(diversity(gen_data2,index="invsimpson"))#### inverse simpson

gen_ind<-gen_ind %>% bind_cols(swd,sid,isd)
reps2<-gen_data%>% dplyr::select(Micro_Rep,Count_Rep)
gen_ind<-bind_cols(gen_ind, reps2)
names(gen_ind)<-c("Light","Phosphorus","Treatment","Shannon","Simpson","Inverse_Simpson","Micro_Rep","Count_Rep")


###### Linear models for each diversity index at the genus level, three tables per diversity index (model output, ANOVA, and random effects)
### Shannon diversity
mod<-lmer(Shannon~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=gen_ind) 
summary (mod)
r.squaredGLMM(mod)
rand(mod) ##random effects table
anova(mod)

### Simpson
mod2<-lmer(Simpson~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=gen_ind)
summary (mod2)
r.squaredGLMM(mod2)
rand(mod2)
anova(mod2)

### Inverse Simpson
mod3<-lmer(Inverse_Simpson~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=gen_ind)
summary (mod3)
r.squaredGLMM(mod3)
rand(mod3)
anova(mod3)

#### Figure 1 in main manuscript, Shannon diversity at the genus level
ggplot(gen_ind,aes(x=Treatment,y=Shannon,color=Light,fill=Light))+
  geom_point(size=2.5)+
  geom_boxplot(alpha=0.5)+
  theme_classic()+
  scale_color_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  scale_fill_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  ylab(label = "Shannon Diversity Index")+
  guides(color=guide_legend(title="Light Color",override.aes = list(fill=c("dodgerblue3","grey25","seagreen","firebrick"))))+
  guides(fill="none")+
  scale_x_discrete(labels=c("Blue_HP"="Blue High","Blue_LP"="Blue Low","Broad_HP"="Broad High","Broad_LP"="Broad Low","Green_HP"="Green High","Green_LP"= "Green Low","Red_HP"="Red High","Red_LP"="Red Low"),guide = guide_axis(angle=45))+
  theme(axis.title =element_text(size=16))+
  theme(axis.text = element_text(size=16,color = "black"))+
  theme(axis.line = element_line(linewidth =1))+
  theme(axis.ticks = element_line(linewidth=1))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=16))
  

#ggsave("shannon_diversity_box.pdf",plot = last_plot(),device = pdf,width = 7,height = 5,dpi=320,units="in")

### genera data
gen_dat<-read_csv("comm_exp_genera.csv")
gen_dat2<-gen_dat%>%pivot_longer(cols=c(9:79),names_to="Taxa",values_to = "biomass",values_drop_na = FALSE)
#write_csv(gen_dat2,"genera_long.csv")

### cyanobacteria type data
all_tax<-unique(gen_dat2$Taxa)%>%
  as_tibble()%>%
  rename(Taxa=value)%>%
  mutate(Cyano_Type="")

all_tax2<-all_tax%>%mutate(Cyano_Type=ifelse(Taxa=="Anabaena"|Taxa=="Jaaginema"|Taxa=="Pseudoanabaena"|Taxa=="Unknown_cyano_filament","Filamentous",Cyano_Type))%>%
  mutate(Cyano_Type=ifelse(Taxa=="Aphanocapsa"|Taxa=="Aphanothece"|Taxa=="Asterococcus"|Taxa=="Cyanodictyon"|Taxa=="Merismopedia"|Taxa=="Synechochoccus"|Taxa=="Unknown_cyano_coccoid"|Taxa=="Unknown_tailed_cyano","Nonfilamentous",Cyano_Type))
  
tax_type<-left_join(gen_dat2,all_tax2,by="Taxa")

tax_type<-tax_type%>%mutate(Cyano_Type=ifelse(Cyano_Type=="","Other",Cyano_Type))
tax_type2<-tax_type%>%filter(Cyano_Type!="Other")%>%
  mutate(Light=ifelse(Light=="Full","Broad",Light))%>%
  rename(Nutrient=Phosphorus)

tax_type2$Treatment[tax_type2$Treatment=="Full_HP"]<-"Broad_HP"
tax_type2$Treatment[tax_type2$Treatment=="Full_LP"]<-"Broad_LP"

#### algal class density data
div_long<-diversity_data%>%pivot_longer(cols = Unknown_Class:Euglena,names_to = "class",values_to = "biomass")

### eug and chrysos have low biomass in all trts, filter out
div_long<-div_long%>%filter(!(class=="Euglena"|class=="Chrysophytes"))


#### Class biomass plot, figure 3 in main MS
ggplot(div_long,aes(x=Phosphorus,y=log(biomass),fill=Light,color=Light ))+
  geom_point(aes(fill=Light),size=3,shape=21,position = position_jitterdodge())+
  geom_boxplot(alpha=0.75)+
  facet_wrap(~class,scales = "free",labeller =labeller(class=c("Green.Algae"="Green Algae","Unknown_Class"="Unknown Class")))+
  scale_color_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  scale_fill_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  theme_classic()+
  labs(x="Nutrient Status",y= "log cell density (cells/mL)")+
  guides(color=guide_legend(title="Light Color",override.aes = list(fill=c("dodgerblue3","grey25","seagreen","firebrick"))))+
  guides(fill="none")+
  theme(strip.text.x = element_text(size=16))+
  theme(strip.background = element_rect(linetype = "blank"))+
  theme(axis.title =element_text(size=20))+
  theme(axis.text = element_text(size=20,color="black"))+
  theme(axis.line = element_line(linewidth =1.5))+
  theme(axis.ticks = element_line(linewidth=1.5))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=16))+
  theme(strip.background = element_rect(fill="white",color="white"))+
  theme(strip.text = element_text(color="black",size = 16))

#ggsave("class_biomass.pdf",plot = last_plot(),device = pdf,width = 9,height = 7,dpi=320, units="in")


######### Figure 4 in main MS, cyanobacteria type
ggplot(tax_type2,aes(x=Treatment,y=log(biomass),fill=Cyano_Type,color=Cyano_Type,shape = Cyano_Type,linetype = Cyano_Type))+
  #geom_dotplot(binaxis = "y")+
  geom_point(aes(fill=Cyano_Type),size=3,position = position_jitterdodge())+
  geom_boxplot(alpha=0.75)+
  #facet_wrap(~Taxa,scales = "free")+
  scale_color_manual(values = c("black","grey50"))+
  scale_fill_manual(values = c("black","grey50"))+
  guides(color=guide_legend(title="Cyanobacteria Type",override.aes = list(fill=c("black","grey50"))))+
  #guides(fill="none",shape="none",linetype="none")+
  guides(fill=guide_legend(title="Cyanobacteria Type"),
         shape=guide_legend(title="Cyanobacteria Type"),
         linetype=guide_legend(title="Cyanobacteria Type"))+
  scale_x_discrete(labels=c("Blue High","Blue Low","Broad High","Broad Low","Green High","Green Low","Red High","Red Low"),guide=guide_axis(angle=45))+
  theme_classic()+
  labs(y="log cell density (cells/mL)")+
  theme(strip.background = element_rect(linetype = "blank"))+
  theme(axis.title =element_text(size=20))+
  theme(axis.text = element_text(size=20,color="black"))+
  theme(axis.line = element_line(linewidth =1.5))+
  theme(axis.ticks = element_line(linewidth=1.5))+
  theme(strip.text = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=16))

#ggsave("cyano_type_biomass.pdf",plot = last_plot(),device = pdf,width = 9,height = 7,dpi=320,units="in")


######### algae class mixed effect models, three table for each class
### cryptophytes
mod4<-lmer(Cryptophytes~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=diversity_data) 
summary(mod4)
r.squaredGLMM(mod4)
rand(mod4)
anova(mod4)

###### cyanobacteria
mod5<-lmer(Cyanobacteria~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=diversity_data) 
summary(mod5)
r.squaredGLMM(mod5)
rand(mod5)
anova(mod5)

########## diatoms
mod6<-lmer(Diatoms~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=diversity_data) 
summary(mod6)
r.squaredGLMM(mod6)
rand(mod6)
anova(mod6)

########## dinoflagellates
mod7<-lmer(Dinoflagellates~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=diversity_data) 
summary(mod7)
r.squaredGLMM(mod7)
rand(mod7)
anova(mod7)

########## green algae
mod8<-lmer(Green.Algae~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=diversity_data) 
summary(mod8)
r.squaredGLMM(mod8)
rand(mod8)
anova(mod8)

########## unknown
mod9<-lmer(Unknown_Class~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=diversity_data) 
summary(mod9)
r.squaredGLMM(mod9)
rand(mod9)
Anova(mod)


######## Cyanobacteria type models, three tables per type
#pivot taxa type wider by cyano type
tax_type3<-tax_type%>%pivot_wider(names_from = Cyano_Type,values_from = biomass)

######filamentous
mod10<-lmer(Filamentous~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=tax_type3) 
summary (mod10)
r.squaredGLMM(mod10)
rand(mod10)
anova(mod10)

#### non-fil
mod11<-lmer(Non_filamentous~Light+Phosphorus+Light*Phosphorus+(1| Micro_Rep/Count_Rep), data=tax_type3) 
summary (mod11)
r.squaredGLMM(mod11)
rand(mod11)
anova(mod11)
