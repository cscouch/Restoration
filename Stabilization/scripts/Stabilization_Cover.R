
rm(list=ls())
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Stabilization/"))

library(dplyr)
library(ggplot2)
library(ggridges)


#LOAD DATA
cover<-read.csv("Stablization_Cover_T0-6monthPO.csv")


levels(as.factor(cover$Survey_Period))
levels(as.factor(cover$Plot_ID))

table(cover$Survey_Period,cover$Treatment)

tmp<- subset(cover,Treatment=="Reference" & Survey_Period=="T1_6mo_preoutplant")
table(tmp$Plot_ID)

tmp2<- subset(cover,Treatment=="Reference" & Survey_Period=="T1_6mo_postoutplant")
table(tmp2$Plot_ID)


# Use baseline control data for post-installation control
pi.c<-cover %>% filter((Survey_Period=="Baseline") & (Treatment=="Control"))
pi.c$Survey_Period <-"T0_Post_Installation"

# Use preoutplant control data for postoutplant control
t1pre.c<-cover %>% filter((Survey_Period=="T1_6mo_preoutplant") & (Treatment=="Control"))
t1pre.c$Survey_Period <-"T1_6mo_postoutplant"

#Combine into dataframe
cover.new<-rbind(pi.c,t1pre.c,cover)

table(cover.new$Survey_Period,cover.new$Treatment)


#Calculate mean rubble size during baseline surveys
rs.baseline<-as.data.frame(cover %>% 
                         filter(Treatment!="Reference")   %>% 
                         filter(Survey_Period=="Baseline")   %>% 
                         group_by(Plot_ID) %>% 
                         summarise(mean.size = mean(Rubble_Size,na.rm=TRUE)))

rs.mean<-as.data.frame(rs.baseline %>% 
                         summarise_each(funs(mean,se=sd(mean.size)/sqrt(n()))))

rs.mean
#Remove reference site data and calculate abudance/plot
col.tot<-as.data.frame(cover.new %>% 
                         #filter(Treatment!="Reference")   %>%                      
                         group_by(Survey_Period, Treatment,Plot_ID) %>% 
                         summarise(n = n()))

View(col.tot)

#Plotting cover abundance for first 2 time points

col.tot$Treatment <- factor(col.tot$Treatment, levels = c("Control", "Mesh", "Boulder","Reference"))
col.tot$Survey_Period <- factor(col.tot$Survey_Period, levels = c("Baseline","T0_Post_Installation","T1_6mo_preoutplant","T1_6mo_postoutplant"))
col.tot$T_SP<-as.factor(paste(col.tot$Survey_Period,col.tot$Treatment,sep="_"))

col.tot %>%
  filter(Survey_Period %in% c("Baseline","T1_6mo_preoutplant")) %>%
  mutate(Survey_Period = recode(Survey_Period, Baseline = 'Baseline', T1_6mo_preoutplant =  'T1 (6months)'))%>%
  filter(T_SP !="Baseline_Reference") %>%
  ggplot(.,aes(Treatment,n))+
  geom_boxplot(aes(fill=Survey_Period))+
  labs(x = "Treatment", y = "cover Abundance")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank()  ) 


###By Genus
col.gen<-as.data.frame(cover %>% 
                         filter(!is.na(Size)) %>%
                         group_by(Survey_Period, Treatment,Plot_ID,cover_Code) %>% 
                         summarise(n = n()))

View(col.gen)

#col.gen<-col.gen %>% filter(Treatment!="Reference")

col.gen$Treatment <- factor(col.gen$Treatment, levels = c("Control", "Mesh", "Boulder","Reference"))
col.gen$T_SP<-as.factor(paste(col.gen$Survey_Period,col.gen$Treatment,sep="_"))

col.gen %>%
  filter(Survey_Period %in% c("Baseline","T1_6mo_preoutplant")) %>%
  filter(T_SP !="Baseline_Reference") %>%
  ggplot(.,aes(Survey_Period,n, fill=cover_Code))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Treatment)+
  labs(x = "Treatment", y = "Colony Abundance")+
  #theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()  )

