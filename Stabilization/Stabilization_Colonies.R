
rm(list=ls())
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Stabilization/"))
lu<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")

library(dplyr)
library(ggplot2)

#LOAD DATA
colony<-read.csv("Stablization_Colony_T0-6monthPO.csv")
colony<- colony %>% 
  rename(SPCODE=Species) %>%
  #mutate(Survey_Period = recode(Survey_Period, T0_Post_Installation = 'T0', Baseline = 'Baseline', T1_6months =  'T1 (6months)'))%>%
  left_join(lu)

levels(as.factor(colony$Survey_Period))
table(colony$Survey_Period,colony$Treatment)

tmp<- subset(colony,Treatment=="Reference" & Survey_Period=="T1_6mo_preoutplant")
table(tmp$Plot_ID)

tmp2<- subset(colony,Treatment=="Reference" & Survey_Period=="T1_6mo_postoutplant")
table(tmp2$Plot_ID)


# Use baseline control data for post-installation control
pi.c<-colony %>% filter((Survey_Period=="Baseline") & (Treatment=="Control"))
pi.c$Survey_Period <-"T0_Post_Installation"

# Use preoutplant control data for postoutplant control
t1pre.c<-colony %>% filter((Survey_Period=="T1_6mo_preoutplant") & (Treatment=="Control"))
t1pre.c$Survey_Period <-"T1_6mo_postoutplant"

#Combine into dataframe
colony.new<-rbind(pi.c,t1pre.c,colony)

table(colony.new$Survey_Period,colony.new$Treatment)

#Remove reference site data and calculate abudance/plot
col.tot<-as.data.frame(colony.new %>% 
  filter(Treatment!="Reference")   %>%                      
  group_by(Survey_Period, Treatment,Plot_ID) %>% 
  summarise(n = n()))

View(col.tot)

#Stats
library(car)

col.tot$sqrt_abun<-sqrt(col.tot$n)
mod <- aov(sqrt_abun ~ Survey_Period * Treatment, data = col.tot)
plot(mod, which = 2)
qqPlot(mod$residuals,id = FALSE)
hist(mod$residuals)
leveneTest(mod)

mod <- aov(sqrt_abun ~ Survey_Period * Treatment, data = col.tot)

library(lme4)
mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
nullmod<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
anova(mod1,nullmod)

mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
mod2<-glmer(n~Survey_Period+(1|Plot_ID), family="poisson",data=col.tot)
anova(mod1,mod2)

mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
mod3<-glmer(n~Treatment+(1|Plot_ID), family="poisson",data=col.tot)
anova(mod1,mod3)

col.tot<-subset(col.tot, Treatment=="Boulder")
mod1<-glmer(n~Survey_Period +(1|Plot_ID), family="poisson",data=col.tot)

library(emmeans)
emmeans(mod1, list(pairwise ~ Survey_Period), adjust = "tukey")


1-logLik(mod1)/logLik(nullmod) # Calculate McFadden's R2 =0.3081


col.tot$Treatment <- factor(col.tot$Treatment, levels = c("Control", "Mesh", "Boulder"))
col.tot$Survey_Period <- factor(col.tot$Survey_Period, levels = c("Baseline","T0_Post_Installation","T1_6mo_preoutplant","T1_6mo_postoutplant"))

ggplot(col.tot,aes(Treatment,n))+
  geom_boxplot(aes(fill=Survey_Period))+
  #geom_jitter(width = 0.2, height = 0, size = 1, alpha = 0.8)
  labs(x = "Treatment", y = "Colony Abundance")+
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()  )



###By Genus
col.gen<-as.data.frame(colony3 %>% 
                         group_by(Survey_Period, Treatment,Plot_ID,GENUS_CODE) %>% 
                         summarise(n = n()))

View(col.gen)

col.gen<-col.gen %>% filter(Treatment!="Reference")

col.gen$Treatment <- factor(col.gen$Treatment, levels = c("Control", "Mesh", "Boulder"))

ggplot(col.gen,aes(Survey_Period,n, fill=GENUS_CODE))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Treatment)+
labs(x = "Treatment", y = "Colony Abundance")+
  #theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()  )
  
