## Biomass and competition models, figures, and tables
## Authors: Cole Vandemark, Viktoria Wagner, and Emily Holden
## Last edited by Emily: May 26, 2025

#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("nlme")
#install.packages("emmeans")
#install.packages("magrittr")
#install.packages("cowplot")
#install.packages("readr")

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(nlme)
library(emmeans)
library(magrittr)
library(cowplot)
library(readr)

setwd("~/Documents/Side Projects/Viktoria and Cole - EICA mesocosm/Cole EICA mesocosm")

### import and clean data ####
#competition data
dataset <- read_rds("output/Competition metrics.rds") %>%
  mutate(Species = case_when(
    str_starts(Pop.target, "Br") ~ "B. inermis",
    str_starts(Pop.target, "Ag") ~ "A. cristatum",
    str_starts(Pop.target, "Po") ~ "P. pratensis subsp. angustifolia"), .before = Pop.target) %>% #add species column
  mutate(Treatment = paste0(Pop.target, "/", Pop.neighbour), .after = Species) %>% 
  rename(region.target = Target.plant,
         Pop.interaction = Pop.neighbour,
         T.above = tolerance.Above,
         T.below = tolerance.Below,
         T.total = tolerance.Total,
         S.above = suppression.Above,
         S.below = suppression.Below,
         S.total = suppression.Total) %>%
  mutate(region.target = if_else(region.target == "non-native", "introduced", region.target)) #change 'non-native' values to 'introduced'
sum(is.na(dataset)) #no NA values
str(dataset)

#alone biomass data
controls <- read_excel("data/Vandemark Comp. Study - Control Data.xlsx", col_names = TRUE) %>%
  dplyr::select(1:4) %>%
  rename(Pop.target = Population,
         Above = 'Aboveground biomass (g) (at harvest)',
         Below = 'Belowground biomass (g) (at harvest)',
         Total = 'Total biomass (g)') %>% 
  mutate(Species = case_when(
    str_starts(Pop.target, "Br") ~ "B.inermis",
    str_starts(Pop.target, "Ag") ~ "A.cristatum",
    str_starts(Pop.target, "Po") ~ "P.angustifolia")) %>% #add species column
  mutate(Pop.target = str_replace(Pop.target, "CAN", "Can")) %>% ## ensure labels match for all populations
  mutate(region.target = if_else(str_detect(Pop.target, "Can"), "introduced", "native")) # add native/non-native status

#### Biomass models for alone plants ####
#### Aboveground biomass ####
#linear mixed model
#lme.ab.biomass <- lme(Above ~ Species, random = ~ 1|Pop.target/Treatment, data = controls) #Cole's code
lme.ab.biomass <- lme(Above ~ Species*region.target, random = ~ 1|Pop.target, data = controls)
summary(lme.ab.biomass)
anova(lme.ab.biomass)
#post hoc tests
#just between species
emmeans(lme.ab.biomass, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.ab.biomass, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.ab.biomass, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.ab.biomass)
shapiro.test(resid)
hist(resid)

#plot
#biomass summary
biomass.summary <- controls %>%
  group_by(Species,region.target) %>%
  summarize(mean.Above = mean(Above),
            se.Above = sd(Above)/sqrt(length(Above)),
            mean.Below = mean(Below),
            se.Below = sd(Below)/sqrt(length(Below)),
            mean.Total = mean(Total),
            se.Total = sd(Total)/sqrt(length(Total)))

# figure
ab.biomass.plot <- ggplot(data=biomass.summary, aes(x=Species,y=mean.Above,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean.Above-se.Above,ymax=mean.Above+se.Above),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=0.5,label="ns",cex=5)+ 
  annotate("text",x=2,y=0.5,label="ns",cex=5)+
  annotate("text",x=3,y=0.5,label="ns",cex=5)+
  #annotate("text",x=2.8,y=2,label="species: p=0.608",cex=5)+ 
  #annotate("text",x=2.9,y=1.9,label="origin: p=0.562",cex=5)+
  #annotate("text",x=2.5,y=1.8,label="species:origin: p=0.057",cex=5)+
  scale_x_discrete(name = " ",  
                   labels = c(expression(italic("A. cristatum")), 
                              expression(italic("B. inermis")), 
                              expression(italic("P. pratensis"))))+
  ggtitle("Aboveground biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
        axis.title = element_text(size = 20))+
  scale_fill_grey(name = "Origin")+
  ylim(0,2)+
  ylab("Mean biomass (g)")
ab.biomass.plot

#### Belowground biomass ####
#linear mixed model
#lme.ab.biomass <- lme(Above ~ Species, random = ~ 1|Pop.target/Treatment, data = controls) #Cole's code
lme.bg.biomass <- lme(Below ~ Species*region.target, random = ~ 1|Pop.target, data = controls)
summary(lme.bg.biomass)
anova(lme.bg.biomass)
#post hoc tests
#just between species
emmeans(lme.bg.biomass, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.bg.biomass, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.bg.biomass, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.bg.biomass)
shapiro.test(resid) #significant
hist(resid)

# figure
bg.biomass.plot <- ggplot(data=biomass.summary, aes(x=Species,y=mean.Below,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean.Below-se.Below,ymax=mean.Below+se.Below),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=0.5,label="ns",cex=5)+ 
  annotate("text",x=2,y=0.6,label="ns",cex=5)+
  annotate("text",x=3,y=1.3,label="**",cex=5)+
  #annotate("text",x=2.93,y=2,label="species: p<0.001",cex=3)+ 
  #annotate("text",x=2.85,y=1.95,label="origin: p=0.046",cex=3)+
  #annotate("text",x=2.55,y=1.9,label="species:origin: p=0.001",cex=3)+
  scale_x_discrete(name = " ",  
                   labels = c(expression(italic("A. cristatum")), 
                              expression(italic("B. inermis")), 
                              expression(italic("P. pratensis"))))+
  ggtitle("Belowground biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
        axis.title = element_text(size = 20))+
  scale_fill_grey(name = "Origin")+
  ylim(0,2)+
  ylab(" ")
bg.biomass.plot

#### Total biomass ####
#linear mixed model
#lme.ab.biomass <- lme(Above ~ Species, random = ~ 1|Pop.target/Treatment, data = controls) #Cole's code
lme.tot.biomass <- lme(Total ~ Species*region.target, random = ~ 1|Pop.target, data = controls)
summary(lme.tot.biomass)
anova(lme.tot.biomass)
#post hoc tests
#just between species
emmeans(lme.tot.biomass, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.tot.biomass, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.tot.biomass, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.tot.biomass)
shapiro.test(resid) #significant
hist(resid)

# figure
tot.biomass.plot <- ggplot(data=biomass.summary, aes(x=Species,y=mean.Total,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean.Total-se.Total,ymax=mean.Total+se.Total),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=0.7,label="ns",cex=5)+ 
  annotate("text",x=2,y=1,label="ns",cex=5)+
  annotate("text",x=3,y=1.7,label="*",cex=5)+
  #annotate("text",x=2.93,y=2,label="species: p<0.001",cex=3)+ 
  #annotate("text",x=2.85,y=1.95,label="origin: p=0.194",cex=3)+
  #annotate("text",x=2.55,y=1.9,label="species:origin: p=0.004",cex=3)+
  scale_x_discrete(name = " ",  
                   labels = c(expression(italic("A. cristatum")), 
                              expression(italic("B. inermis")), 
                              expression(italic("P. pratensis"))))+
  ggtitle("Total biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_fill_grey(name = "Origin")+
  ylim(0,2)+
  ylab(" ")
tot.biomass.plot

#### put biomass plots together ####
legend <- cowplot::get_legend(tot.biomass.plot)
biomass.plots <- cowplot::plot_grid(ab.biomass.plot + theme(legend.position = "none"), 
                                    bg.biomass.plot + theme(legend.position = "none"), 
                                    tot.biomass.plot + theme(legend.position = "none"),
                                    labels = "auto",
                                    nrow = 1, align = "hv")
final.biomass.plot <- cowplot::plot_grid(biomass.plots, legend, nrow = 1, rel_widths = c(1, 0.2))
final.biomass.plot
final.biomass.plot.with.label <- plot_grid(
  final.biomass.plot,
  ggdraw() + draw_label("Species", size = 20),
  ncol = 1,
  rel_heights = c(1, 0.05)
)
final.biomass.plot.with.label

##export
ggsave(filename = "Cole's biomass plots.png", 
       final.biomass.plot.with.label,
       path = "figures/",
       width = 16,
       height = 8,
       units = "in"
)

#### Tolerance models ####
#### Tolerance.above ####
#linear mixed model
lme.t.ab <- lme(T.above ~ Species*region.target, random = ~ 1|Pop.target/Treatment, data = dataset)
summary(lme.t.ab)
anova(lme.t.ab)
#post hoc tests
#just between species
emmeans(lme.t.ab, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.t.ab, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.t.ab, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.t.ab)
shapiro.test(resid)
hist(resid)

#plot
#T.above summary
T.above.summary <- dataset %>%
  group_by(Species,region.target) %>%
  summarise(se=sd(T.above)/sqrt(n()),
            mean=mean(T.above))

aboveground.tolerance.plot <- ggplot(data=T.above.summary,aes(x=Species,y=mean,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=-1.2,label="ns",cex=5)+ 
  annotate("text",x=2,y=-1.2,label="ns",cex=5)+
  annotate("text",x=3,y=-1.2,label="ns",cex=5)+
  #annotate("text",x=2.93,y=1,label="species: p=0.801",cex=3)+ 
  #annotate("text",x=2.85,y=0.9,label="origin: p=0.798",cex=3)+
  #annotate("text",x=2.55,y=0.8,label="species:origin: p=0.137",cex=3)+
  scale_x_discrete(name = " ",  labels = c(expression(italic("A. cristatum")), 
                                           expression(italic("B. inermis")), 
                                           expression(italic("P. pratensis"))))+
  ggtitle("Aboveground biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_fill_grey(name = "Origin")+
  ylim(-2.1,1)+
  ylab("Mean tolerance")
aboveground.tolerance.plot

#### Tolerance.below ####
#linear mixded model
lme.t.bg <- lme(T.below ~ Species*region.target, random = ~ 1|Pop.target/Treatment, data = dataset)
summary(lme.t.bg)
anova(lme.t.bg)
#post hoc tests
#just between species
emmeans(lme.t.bg, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.t.bg, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.t.bg, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.t.bg)
shapiro.test(resid)
hist(resid) #not too bad

#plot
#T.above summary
T.below.summary <- dataset %>%
  group_by(Species,region.target) %>%
  summarise(se=sd(T.below)/sqrt(n()),
            mean=mean(T.below))


belowground.tolerance.plot <- ggplot(data=T.below.summary,aes(x=Species,y=mean,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=-1,label="ns",cex=5)+ 
  annotate("text",x=2,y=-1,label="ns",cex=5)+
  annotate("text",x=3,y=-2,label="**",cex=5)+
  #annotate("text",x=2.93,y=1,label="species: p=0.060",cex=3)+ 
  #annotate("text",x=2.85,y=0.9,label="origin: p=0.045",cex=3)+
  #annotate("text",x=2.55,y=0.8,label="species:origin: p=0.004",cex=3)+
  scale_x_discrete(name = " ", labels = c(expression(italic("A. cristatum")), 
                                          expression(italic("B. inermis")), 
                                          expression(italic("P. pratensis"))))+
  ggtitle("Belowground biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_fill_grey(name = "Origin")+
  ylim(-2.1, 1) +
  ylab(" ")
belowground.tolerance.plot

#### Tolerance.total ####
#linear mixed model
lme.t.total <- lme(T.total ~ Species*region.target, random = ~ 1|Pop.target/Treatment, data = dataset)
summary(lme.t.total)
anova(lme.t.total)
#post hoc tests
#just between species
emmeans(lme.t.total, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.t.total, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.t.total, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.t.total)
shapiro.test(resid)
hist(resid)

#plot
#T.above summary
T.total.summary <- dataset %>%
  group_by(Species,region.target) %>%
  summarise(se=sd(T.total)/sqrt(n()),
            mean=mean(T.total))


total.tolerance.plot <- ggplot(data=T.total.summary,aes(x=Species,y=mean,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=-1,label="ns",cex=5)+ 
  annotate("text",x=2,y=-1,label="ns",cex=5)+
  annotate("text",x=3,y=-1.75,label="*",cex=5)+
  #annotate("text",x=2.93,y=1,label="species: p=0.215",cex=3)+ 
  #annotate("text",x=2.85,y=0.9,label="origin: p=0.148",cex=3)+
  #annotate("text",x=2.55,y=0.8,label="species:origin: p=0.014",cex=3)+
  scale_x_discrete(name = " ",  labels = c(expression(italic("A. cristatum")), 
                                           expression(italic("B. inermis")), 
                                           expression(italic("P. pratensis"))))+
  ggtitle("Total biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_fill_grey("Origin")+
  ylim(-2.1, 1)+
  ylab(" ")
total.tolerance.plot


####Suppression ####
#### Suppression above ####
#linear mixded model
lme.s.above <- lme(S.above ~ Species*region.target, random = ~ 1|Pop.target/Treatment, data = dataset)
summary(lme.s.above)
anova(lme.s.above)
#post hoc tests
#just between species
emmeans(lme.s.above, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.s.above, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.s.above, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.s.above)
shapiro.test(resid)
hist(resid)


#plot
#T.above summary
S.above.summary <- dataset %>%
  group_by(Species,region.target) %>%
  summarise(se=sd(S.above)/sqrt(length(S.above)),
            mean=mean(S.above))


suppression.above.plot <- ggplot(data=S.above.summary,aes(x=Species,y=mean,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=-1.2,label="ns",cex=5)+ 
  annotate("text",x=2,y=-1.2,label="ns",cex=5)+
  annotate("text",x=3,y=-1.2,label="ns",cex=5)+
  #annotate("text",x=2.93,y=1,label="species: p=0.526",cex=3)+ 
  #annotate("text",x=2.85,y=0.9,label="origin: p=0.662",cex=3)+
  #annotate("text",x=2.55,y=0.8,label="species*origin: p=0.010",cex=3)+
  scale_x_discrete(name = " ",  labels = c(expression(italic("A. cristatum")), 
                                           expression(italic("B. inermis")), 
                                           expression(italic("P. pratensis")))) +
  ggtitle("Aboveground biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_fill_grey("Origin")+
  ylim(-2.1, 1)+
  ylab("Mean suppression")
suppression.above.plot

#### Suppression below ####
#linear mixded model
lme.s.below <- lme(S.below ~ Species*region.target, random = ~ 1|Pop.target/Treatment, data = dataset)
summary(lme.s.below)
anova(lme.s.below)
#post hoc tests
#just between species
emmeans(lme.s.below, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.s.below, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.s.below, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.s.below)
shapiro.test(resid)
hist(resid) #not the worst.

#plot
#T.above summary
S.below.summary <- dataset %>%
  group_by(Species,region.target) %>%
  summarise(se=sd(S.below)/sqrt(n()),
            mean=mean(S.below))

suppression.below.plot <- ggplot(data=S.below.summary,aes(x=Species,y=mean,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=-1,label="ns",cex=5)+ 
  annotate("text",x=2,y=-1,label="ns",cex=5)+
  annotate("text",x=3,y=-2,label="***",cex=5)+
  #annotate("text",x=2.9,y=1,label="species: p=0.005",cex=3)+ 
  #annotate("text",x=2.85,y=0.9,label="origin: p=0.005",cex=3)+
  #annotate("text",x=2.55,y=0.8,label="species:origin: p<0.001",cex=3)+
  scale_x_discrete(name = " ",  labels = c(expression(italic("A. cristatum")), 
                                           expression(italic("B. inermis")), 
                                           expression(italic("P. pratensis")))) +
  ggtitle("Belowground biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_fill_grey("Origin")+
  ylim(-2.1, 1)+
  ylab(" ")
suppression.below.plot

#### Suppression total ####
#linear mixded model
lme.s.total <- lme(S.total ~ Species*region.target, random = ~ 1|Pop.target/Treatment, data = dataset)
summary(lme.s.total)
anova(lme.s.total)
#post hoc tests
#just between species
emmeans(lme.s.total, list(pairwise ~ Species), adjust = "tukey", data = dataset)
#species region target
emmeans(lme.s.total, list(pairwise ~ Species:region.target), adjust = "tukey", data = dataset)
#region target
emmeans(lme.s.total, list(pairwise ~ region.target), adjust = "tukey", data = dataset)

#Residual normality test
resid <- residuals(lme.s.total)
shapiro.test(resid)
hist(resid)

#plot
#T.above summary
S.total.summary <- dataset %>%
  group_by(Species,region.target) %>%
  summarise(se=sd(S.total)/sqrt(n()),
            mean=mean(S.total))

suppression.total.plot <- ggplot(data=S.total.summary,aes(x=Species,y=mean,fill=region.target)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2,position = position_dodge(0.9))+
  annotate("text",x=1,y=-1,label="ns",cex=5)+ 
  annotate("text",x=2,y=-1,label="ns",cex=5)+
  annotate("text",x=3,y=-1.7,label="**",cex=5)+
  #annotate("text",x=2.93,y=1,label="species: p=0.031",cex=3)+ 
  #annotate("text",x=2.85,y=0.9,label="origin: p=0.027",cex=3)+
  #annotate("text",x=2.55,y=0.8,label="species:origin: p<0.001",cex=3)+
  scale_x_discrete(name = " ",  labels = c(expression(italic("A. cristatum")), 
                                           expression(italic("B. inermis")), 
                                           expression(italic("P. pratensis")))) +
  ggtitle("Total biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_fill_grey("Origin")+
  ylim(-2.1, 1)+
  ylab(" ")
suppression.total.plot

#### put competition plots together ####
competition.plots <- cowplot::plot_grid(aboveground.tolerance.plot + theme(legend.position = "none") + theme(axis.title.x = element_blank()), 
                                        belowground.tolerance.plot + theme(legend.position = "none") + theme(axis.title.x = element_blank()), 
                                        total.tolerance.plot + theme(legend.position = "none")  + theme(axis.title.x = element_blank()),
                                        suppression.above.plot + theme(legend.position = "none") + theme(axis.title.x = element_blank()), 
                                        suppression.below.plot + theme(legend.position = "none") + theme(axis.title.x = element_blank()), 
                                        suppression.total.plot + theme(legend.position = "none") + theme(axis.title.x = element_blank()),
                                        labels = "auto",
                                        nrow = 2, align = "hv")
final.competition.plot <- cowplot::plot_grid(competition.plots, legend, nrow = 1, rel_widths = c(1, 0.2))
final.competition.plot.with.label <- plot_grid(
  final.competition.plot,
  ggdraw() + draw_label("Species", size = 20),
  ncol = 1,
  rel_heights = c(1, 0.05)
)
final.competition.plot.with.label

##export
ggsave(filename = "Cole's competition plots.png", 
       final.competition.plot.with.label,
       path = "figures/",
       width = 16,
       height = 16,
       units = "in"
)

#### make results tables ####
#### anova tables ####
# Define the row labels you want
model_list <- list(
  lme.ab.biomass = lme.ab.biomass,
  lme.bg.biomass = lme.bg.biomass,
  lme.tot.biomass = lme.tot.biomass,
  lme.t.ab = lme.t.ab,
  lme.t.bg = lme.t.bg,
  lme.t.total = lme.t.total,
  lme.s.above = lme.s.above,
  lme.s.below = lme.s.below,
  lme.s.total = lme.s.total
)

##Write function
extract_anova_info <- function(model, model_name) {
  anova_table <- anova(model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Term")
  
  # Replace row names with desired labels if length matches
  if (nrow(anova_table) == length(desired_labels)) {
    anova_table$Term <- desired_labels
  }
  
  # Add model name
  anova_table$Model <- model_name
  
  # Select and rename columns
  anova_table <- anova_table %>%
    select(
      Model,
      Term,
      `Numerator df` = numDF,
      `Denominator df` = denDF,
      `F value` = `F-value`,
      `p value` = `p-value`
    )
  
  # Convert p-values to character to handle them as strings
  anova_table$`p value` <- as.character(anova_table$`p value`)
  
  # Replace "<.0001" with "<0.001" directly (no rounding for this case)
  anova_table$`p value` <- ifelse(
    anova_table$`p value` == "<.0001",
    "<0.001",  # If "<.0001", set to "<0.001"
    anova_table$`p value`  # Otherwise, keep it as is
  )
  
  # Round numeric columns (for actual numeric p-values, if needed)
  anova_table <- anova_table %>%
    mutate(
      `p value` = case_when(
        !grepl("<", anova_table$`p value`) ~ format(round(as.numeric(anova_table$`p value`), 3), nsmall = 3),
        TRUE ~ anova_table$`p value`  # Keep the "<0.001" or other p-value strings unchanged
      )
    )
  
  # Round other numeric columns (Numerator df, Denominator df, F value)
  anova_table <- anova_table %>%
    mutate(across(c(`Numerator df`, `Denominator df`, `F value`), ~ round(.x, 3)))
  
  return(anova_table)
}


# Apply function to all models in list
results <- map2_dfr(model_list, names(model_list), extract_anova_info)

# Export to CSV
write_csv(results, "output/lme_anova_results.csv")

#### post-hoc test tables ####
## write function to extract pairwise comparisons
extract_posthoc_info <- function(model, model_name, data) {
  # Run emmeans pairwise comparisons
  emmeans_result <- emmeans(model, pairwise ~ Species:region.target, adjust = "tukey", data = data)
  
  # Get pairwise comparisons
  posthoc_table <- summary(emmeans_result$contrasts) %>%
    as.data.frame()
  
  # Format p-values
  posthoc_table$`p value` <- ifelse(
    posthoc_table$p.value < 0.001,
    "<0.001",
    format(round(posthoc_table$p.value, 3), nsmall = 3)
  )
  
  # Add model name
  posthoc_table$Model <- model_name
  
  # Select and format columns
  posthoc_table <- posthoc_table %>%
    select(
      Model,
      Contrast = contrast,
      Estimate = estimate,
      SE = SE,
      DF = df,
      `t ratio` = t.ratio,
      `p value`
    ) %>%
    mutate(across(c(Estimate, SE, DF, `t ratio`), ~ round(.x, 3)))
  
  return(posthoc_table)
}

# Apply the function across models
posthoc_results <- map2_dfr(model_list, names(model_list), ~ extract_posthoc_info(.x, .y, data = dataset))

# filter the results to include only within-species comparisons
filtered_results <- posthoc_results %>%
  filter(str_extract(Contrast, "^[^ ]+") == str_extract(Contrast, "(?<= - )[^ ]+"))

# Export to CSV
write_csv(filtered_results, "output/posthoc_results.csv")
