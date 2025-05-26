## Compare biomass for plants grown alone and with neighbours
## Author: Emily H
## Created: April 21, 2025
## Last edited: May 26, 2025

#install.packages("tidyverse")

library(tidyverse)
library(readxl)

setwd("~/Documents/Side Projects/Viktoria and Cole - EICA mesocosm/Cole EICA mesocosm")

#### import data ####
data <- read_excel("data/Vandemark_Competition plants_Spreadsheet.xlsx", col_names = TRUE) %>%
  rename(Target.plant = 'Target plant',
         target.Above = 'Target aboveground biomass (g)',
         target.Below = 'Target belowground biomass (g)',
         target.Total = 'Target total biomass (g)',
         neighbour.Above = 'Neighbour aboveground biomass (g)',
         neighbour.Below = 'Neighbour belowground biomass (g)',
         neighbour.Total = 'Neighbour total biomass (g)') %>%
  mutate(Treatment = str_replace(Treatment, "CAN", "Can")) %>% ## ensure labels match for all populations
  separate(Treatment, into = c("Pop1", "Pop2"), sep = "/") %>%
  mutate(Pop.target = if_else(Target.plant == "non-native", Pop2, Pop1), .before = target.Above) %>% #create column tracking the population of the target plant.
  mutate(Pop.neighbour = if_else(Target.plant == "native", Pop2, Pop1), .before = target.Above) %>% #create column tracking the population of the target plant.
  dplyr::select(-Pop1, -Pop2)  # Remove intermediate columns when not needed
sum(is.na(data)) #no NA values
str(data)

## import control (alone) plant data 
controls <- read_excel("data/Vandemark Comp. Study - Control Data.xlsx", col_names = TRUE) %>%
  dplyr::select(1:4) %>%
  mutate(Population = str_replace(Population, "CAN", "Can")) %>% ## ensure labels match for all populations
  rename(Pop.target = Population,
         Above = 'Aboveground biomass (g) (at harvest)',
         Below = 'Belowground biomass (g) (at harvest)',
         Total = 'Total biomass (g)')
str(controls)

#### add grouping variables and summarize ####
controls.summary <- controls %>% 
  mutate(Species = case_when(
    startsWith(Pop.target, "Br") ~ "B. inermis",
    startsWith(Pop.target, "Po") ~ "P. pratensis",
    startsWith(Pop.target, "Ag") ~ "A. cristatum")) %>% #create new column for species identity
  group_by(Species) %>%
  summarize(mean.Above = mean(Above),
            se.Above = sd(Above)/sqrt(length(Above)),
            mean.Below = mean(Below),
            se.Below = sd(Below)/sqrt(length(Below)),
            mean.Total = mean(Total),
            se.Total = sd(Total)/sqrt(length(Total))) %>% 
  mutate(Neighbours = "alone", .before = mean.Above) #create column to identify these are alone plants
str(controls.summary)

neighbours.summary <- data %>% 
  mutate(Species = case_when(
    startsWith(Pop.target, "Br") ~ "B. inermis",
    startsWith(Pop.target, "Po") ~ "P. pratensis",
    startsWith(Pop.target, "Ag") ~ "A. cristatum")) %>% #create new column for species identity
  group_by(Species) %>%
  summarize(mean.Above = mean(target.Above),
            se.Above = sd(target.Above)/sqrt(length(target.Above)),
            mean.Below = mean(target.Below),
            se.Below = sd(target.Below)/sqrt(length(target.Below)),
            mean.Total = mean(target.Total),
            se.Total = sd(target.Total)/sqrt(length(target.Total))) %>% 
  mutate(Neighbours = "with neighbour", .before = mean.Above) #create column to identify these are alone plants
str(neighbours.summary)

##bind dfs
growth.comparison.df <- rbind(controls.summary, neighbours.summary)

#### make table for export ####
tableS3 <- growth.comparison.df %>% 
  rename('Neighbour status' = Neighbours) %>%
  mutate('Mean aboveground biomass (g)' = paste(round(mean.Above,3), "±", round(se.Above,3)),
         'Mean belowground biomass (g)' = paste(round(mean.Below,3), "±", round(se.Below,3)),
         'Mean total biomass (g)' = paste(round(mean.Total,3), "±", round(se.Total,3))) %>%
  dplyr::select(Species, 'Neighbour status', 'Mean aboveground biomass (g)', 'Mean belowground biomass (g)', 'Mean total biomass (g)')
write_csv(tableS3, "output/alone vs with neigbours biomass comparison.csv")

#### make figure S1 #### 
## aboveground biomass plot
aboveground.biomass.comparison <- ggplot(data=growth.comparison.df, aes(x=Species,y=mean.Above,fill=Neighbours)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean.Above-se.Above,ymax=mean.Above+se.Above),width=0.2,position = position_dodge(0.9))+
  scale_x_discrete(name = "Species",  
                   labels = c(expression(italic("A. cristatum")), 
                              expression(italic("B. inermis")), 
                              expression(italic("P. pratensis"))))+
  ggtitle("Aboveground biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_grey(name = "Neighbour status")+
  ylim(0,1.5)+
  ylab("Mean biomass (g)")
aboveground.biomass.comparison

## belowground biomass plot
belowground.biomass.comparison <- ggplot(data=growth.comparison.df, aes(x=Species,y=mean.Below,fill=Neighbours)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean.Below-se.Below,ymax=mean.Below+se.Below),width=0.2,position = position_dodge(0.9))+
  scale_x_discrete(name = "Species",  
                   labels = c(expression(italic("A. cristatum")), 
                              expression(italic("B. inermis")), 
                              expression(italic("P. pratensis"))))+
  ggtitle("Belowground biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_grey(name = "Neighbour status")+
  ylim(0,1.5) + 
  ylab(" ")
belowground.biomass.comparison

## total biomass plot
total.biomass.comparison <- ggplot(data=growth.comparison.df, aes(x=Species,y=mean.Total,fill=Neighbours)) +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_errorbar(aes(ymin=mean.Total-se.Total,ymax=mean.Total+se.Total),width=0.2,position = position_dodge(0.9))+
  scale_x_discrete(name = "Species",  
                   labels = c(expression(italic("A. cristatum")), 
                              expression(italic("B. inermis")), 
                              expression(italic("P. pratensis"))))+
  ggtitle("Total biomass")+
  theme_classic(base_size = 20) +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_grey(name = "Neighbour status")+
  ylim(0,1.5)+
  ylab(" ")
total.biomass.comparison

#### put  plots together ####
biomass.legend <- cowplot::get_legend(total.biomass.comparison)
neighbour.biomass.plots <- cowplot::plot_grid(aboveground.biomass.comparison + theme(legend.position = "none"), 
                                              belowground.biomass.comparison + theme(legend.position = "none"), 
                                              total.biomass.comparison + theme(legend.position = "none"),
                                              labels = "auto",
                                              nrow = 1, align = "hv")
final.neighbour.biomass.plots <- cowplot::plot_grid(neighbour.biomass.plots, biomass.legend, nrow = 1, rel_widths = c(1, 0.2))
final.neighbour.biomass.plots

##export
ggsave(filename = "Neighbour biomass plots.png", 
       final.neighbour.biomass.plots,
       path = "figures/",
       width = 16,
       height = 6,
       units = "in"
)
