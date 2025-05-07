
rm(list=ls())
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Stabilization/"))
lu<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")

library(dplyr)
library(ggplot2)

#LOAD DATA
colony<-read.csv("Stablization_Colony_04172025.csv")
colony<- colony %>% select (-c(Notes:X.14))
colony<- rename(colony, SPCODE=Juvenile_Code)
colony<-left_join(colony,lu)

bl.c<-colony %>% filter((Survey.Period=="Baseline") & (Treatment=="Control"))
bl.c$Survey.Period<-"T0_Post_Installation"
colony2<-colony %>% filter(Survey.Period!="Baseline")

colony3<-rbind(bl.c,colony2)

col.tot<-as.data.frame(colony3 %>% 
  group_by(Survey.Period, Treatment,Plot_ID) %>% 
  summarise(n = n()))

View(col.tot)

col.tot<-col.tot %>% filter(Treatment!="Reference")

col.tot$Treatment <- factor(col.tot$Treatment, levels = c("Control", "Mesh", "Boulder"))

ggplot(col.tot,aes(Treatment,n))+
  geom_boxplot(aes(fill=Survey.Period))+
  #geom_jitter(width = 0.2, height = 0, size = 1, alpha = 0.8)
  labs(x = "Treatment", y = "Colony Abundance")+
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()  )



###By Genus
col.gen<-as.data.frame(colony3 %>% 
                         group_by(Survey.Period, Treatment,Plot_ID,GENUS_CODE) %>% 
                         summarise(n = n()))

View(col.gen)

col.gen<-col.gen %>% filter(Treatment!="Reference")

col.gen$Treatment <- factor(col.gen$Treatment, levels = c("Control", "Mesh", "Boulder"))

ggplot(col.gen,aes(Survey.Period,n, fill=GENUS_CODE))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Treatment)+
labs(x = "Treatment", y = "Colony Abundance")+
  #theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()  )
  
