
rm(list=ls())
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Stabilization/"))

library(dplyr)
library(ggplot2)
library(ggridges)


#LOAD DATA
urchin<-read.csv("data/Stablization_Urchins_T0-6monthPO.csv")
# urchin<- urchin %>% 
#   rename(SPCODE=Species) %>%
#   #mutate(Survey_Period = recode(Survey_Period, T0_Post_Installation = 'T0', Baseline = 'Baseline', T1_6months =  'T1 (6months)'))%>%
#   left_join(lu)

levels(as.factor(urchin$Survey_Period))
levels(as.factor(urchin$Plot_ID))

table(urchin$Survey_Period,urchin$Treatment)

tmp<- subset(urchin,Treatment=="Reference" & Survey_Period=="T1_6mo_preoutplant")
table(tmp$Plot_ID)

tmp2<- subset(urchin,Treatment=="Reference" & Survey_Period=="T1_6mo_postoutplant")
table(tmp2$Plot_ID)


# Use baseline control data for post-installation control
pi.c<-urchin %>% filter((Survey_Period=="Baseline") & (Treatment=="Control"))
pi.c$Survey_Period <-"T0_Post_Installation"

# Use preoutplant control data for postoutplant control
t1pre.c<-urchin %>% filter((Survey_Period=="T1_6mo_preoutplant") & (Treatment=="Control"))
t1pre.c$Survey_Period <-"T1_6mo_postoutplant"

#Combine into dataframe
urchin.new<-rbind(pi.c,t1pre.c,urchin)

table(urchin.new$Survey_Period,urchin.new$Treatment)

#Remove reference site data and calculate abudance/plot
col.tot<-as.data.frame(urchin.new %>% 
                         #filter(Treatment!="Reference")   %>%                      
                         group_by(Survey_Period, Treatment,Plot_ID) %>% 
                         summarise(n = n()))

View(col.tot)


#Save file for Nyssa to use for Bayasian analysis- remove reference site and post outplant data
for.nyssa<-as.data.frame(urchin.new %>% 
                           filter(Treatment!="Reference")   %>%
                           filter(Survey_Period!="T1_6mo_postoutplant")   %>%
                           group_by(Survey_Period, Treatment,Plot_ID) %>% 
                           summarise(urchin_abun = n()))

#save file
write.csv(for.nyssa,file="data for Nyssa/Urchins_byPlot_forNyssa.csv",row.names = FALSE)



#Plotting Urchin abundance for first 2 time points

col.tot$Treatment <- factor(col.tot$Treatment, levels = c("Control", "Mesh", "Boulder","Reference"))
col.tot$Survey_Period <- factor(col.tot$Survey_Period, levels = c("Baseline","T0_Post_Installation","T1_6mo_preoutplant","T1_6mo_postoutplant"))
col.tot$T_SP<-as.factor(paste(col.tot$Survey_Period,col.tot$Treatment,sep="_"))
  
col.tot %>%
  filter(Survey_Period %in% c("Baseline","T1_6mo_preoutplant")) %>%
  mutate(Survey_Period = recode(Survey_Period, Baseline = 'Baseline', T1_6mo_preoutplant =  'T1 (6months)'))%>%
  filter(T_SP !="Baseline_Reference") %>%
ggplot(.,aes(Treatment,n))+
  geom_boxplot(aes(fill=Survey_Period))+
  labs(x = "Treatment", y = "Urchin Abundance")+
theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),  
        legend.position="bottom") 


###By Genus
col.gen<-as.data.frame(urchin %>% 
                         filter(!is.na(Size)) %>%
                         group_by(Survey_Period, Treatment,Plot_ID,Urchin_Code) %>% 
                         summarise(n = n()))

View(col.gen)

#col.gen<-col.gen %>% filter(Treatment!="Reference")

col.gen$Treatment <- factor(col.gen$Treatment, levels = c("Control", "Mesh", "Boulder","Reference"))
col.gen$T_SP<-as.factor(paste(col.gen$Survey_Period,col.gen$Treatment,sep="_"))

col.gen %>%
  filter(Survey_Period == "T1_6mo_preoutplant") %>%
  filter(Treatment %in% c("Boulder","Reference")) %>%
  filter(T_SP !="Baseline_Reference") %>%
ggplot(.,aes(Treatment,n, fill=Urchin_Code))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Treatment", y = "Urchin Abundance")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),  
        legend.position="bottom") 
