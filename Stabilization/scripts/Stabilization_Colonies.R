#Script summarizes wild colonies on experimental and reference site

rm(list=ls())
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Stabilization/"))
lu<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")

library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyr)



#LOAD DATA
colony<-read.csv("data/Stablization_Colony_T0-6monthPO.csv")
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


#Save file for Nyssa to use for Bayasian analysis- remove reference site and post outplant data
for.nyssa<-as.data.frame(colony.new %>% 
                         filter(Treatment!="Reference")   %>%
                         filter(Survey_Period!="T1_6mo_postoutplant")   %>%
                         group_by(Survey_Period, Treatment,Plot_ID) %>% 
                         summarise(colony_abun = n()))

#save file
write.csv(for.nyssa,file="data/WildColony_byPlot_forNyssa.csv",row.names = FALSE)

#Stats
# library(car)
# 
# col.tot$sqrt_abun<-sqrt(col.tot$n)
# mod <- aov(sqrt_abun ~ Survey_Period * Treatment, data = col.tot)
# plot(mod, which = 2)
# qqPlot(mod$residuals,id = FALSE)
# hist(mod$residuals)
# leveneTest(mod)
# 
# mod <- aov(sqrt_abun ~ Survey_Period * Treatment, data = col.tot)
# 
# library(lme4)
# mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# nullmod<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# anova(mod1,nullmod)
# 
# mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# mod2<-glmer(n~Survey_Period+(1|Plot_ID), family="poisson",data=col.tot)
# anova(mod1,mod2)
# 
# mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# mod3<-glmer(n~Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# anova(mod1,mod3)
# 
# col.tot<-subset(col.tot, Treatment=="Boulder")
# mod1<-glmer(n~Survey_Period +(1|Plot_ID), family="poisson",data=col.tot)
# 
# library(emmeans)
# emmeans(mod1, list(pairwise ~ Survey_Period), adjust = "tukey")
# 
# 
# 1-logLik(mod1)/logLik(nullmod) # Calculate McFadden's R2 =0.3081


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

col.tot<-as.data.frame(colony.new %>% 
                         filter(Treatment!="Reference")   %>%                      
                         group_by(Survey_Period, Treatment,Plot_ID) %>% 
                         summarise(n = n()))

col.gen<-as.data.frame(colony.new %>% 
                         filter(Treatment!="Reference")   %>%  
                         filter(Survey_Period!="T1_6mo_postoutplant")   %>%  
                         group_by(Survey_Period, Treatment,Plot_ID,GENUS_CODE) %>% 
                         summarise(n = n()))

View(col.gen)

# Reshape the data to wide format: one column per survey period- filter just boulder piles
col.gen.wide <- col.gen %>%
  filter(Treatment =="Boulder")   %>%  
  filter(Survey_Period!="Baseline")   %>%  
  mutate(Genus = recode(GENUS_CODE, MOSP = 'Montipora', POCS = 'Pocillopora', POSP =  'Porites'))%>%
  pivot_wider(names_from = Survey_Period, values_from = n)

# Step 3: Calculate percent change between the two survey periods
perc.change <- col.gen.wide %>%
  mutate(
    Percent_Change = 100 * ((`T1_6mo_preoutplant` - `T0_Post_Installation`) / `T0_Post_Installation`))%>%
    filter(Genus %in% c("Montipora","Pocillopora","Porites"))
  
#Plot percent change for top 3 taxa 
ggplot(perc.change,aes(Genus,Percent_Change))+
  geom_boxplot(fill = "grey")+
  labs(x = "Genus", y = "% Change in Abundance")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none") 


col.gen$Treatment <- factor(col.gen$Treatment, levels = c("Control", "Mesh", "Boulder"))

ggplot(col.gen,aes(Survey_Period,n, fill=GENUS_CODE))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Treatment)+
labs(x = "Treatment", y = "Colony Abundance")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none") 

#Calculate % change in colony abun by genus. 


#Ridge plot of colony size
size.mean<-as.data.frame(colony.new %>% 
                        filter(Survey_Period =="Baseline" & Treatment %in% c("Control","Reference")) %>%
                        mutate(Treatment = recode(Treatment, Control = 'Experimental Site', Reference = 'Reference Site'))%>%
                        filter(!is.na(Size)) %>%
                        group_by(Survey_Period, Treatment) %>% 
                        summarise(mean = mean(Size),max=max(Size)))
size.mean

p1<-colony.new %>% 
  filter(Survey_Period == "Baseline" & Treatment %in% c("Control", "Reference")) %>%
  mutate(Treatment = recode(Treatment,
                            Control = 'Experimental Site',
                            Reference = 'Reference Site'),
         Treatment = factor(Treatment, levels = c("Reference Site","Experimental Site"))) %>%
  ggplot(aes(x = Size, y = Treatment, fill = Treatment)) +
  geom_density_ridges_gradient(
    scale = 2,
    gradient_lwd = 0.5,
    color = "black",
    rel_min_height = 0
  ) +
  geom_point(shape = 18, size = 4,
             data = size.mean,
             aes(x = mean, y = Treatment),
             inherit.aes = FALSE, fill = "black") +
  scale_fill_manual(values = c("Reference Site" = "#ff7f0e","Experimental Site" = "#1f77b4")) +  
  labs(x = "Colony Size (cm)", y="") +
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 14,face="bold"),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    coord_cartesian(clip="off")
  )


# Use a graphics device to capture both ggplot + grid overlay and add vertical line between facets
jpeg("Colony size ridge plot.jpg", width = 8.5, height = 6, units = "in", res = 300)

# Draw the ggplot
print(p1)

# Close the graphics device
dev.off()