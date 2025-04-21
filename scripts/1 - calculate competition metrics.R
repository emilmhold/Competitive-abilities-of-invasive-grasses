## Calculate competitive suppression and tolerance
## Author: Emily H
## Created: March 28, 2025
## Last edited: April 21, 2025

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

##find max and min biomass values
min(data$target.Total)
max(data$target.Total)

## import control (alone) plant data 
controls <- read_excel("data/Vandemark Comp. Study - Control Data.xlsx", col_names = TRUE) %>%
  dplyr::select(1:4) %>%
  mutate(Population = str_replace(Population, "CAN", "Can")) %>% ## ensure labels match for all populations
  rename(Pop.target = Population,
         Above = 'Aboveground biomass (g) (at harvest)',
         Below = 'Belowground biomass (g) (at harvest)',
         Total = 'Total biomass (g)')
str(controls)

## calculate mean biomass response for target plants
pop.means <- controls %>%
  group_by(Pop.target) %>% #group by the source population
  summarize(mean.Above = mean(Above),
            se.Above = sd(Above)/sqrt(length(Above)),
            mean.Below = mean(Below),
            se.Below = sd(Below)/sqrt(length(Below)),
            mean.Total = mean(Total),
            se.Total = sd(Total)/sqrt(length(Total)))
str(pop.means)

## calculate mean biomass response for neighbour plants
  ###same script as above but with different column names
neighbour.means <- controls %>%
  group_by(Pop.target) %>% #group by the source population
  summarize(neighbour.mean.Above = mean(Above),
            neighbour.se.Above = sd(Above)/sqrt(length(Above)),
            neighbour.mean.Below = mean(Below),
            neighbour.se.Below = sd(Below)/sqrt(length(Below)),
            neighbour.mean.Total = mean(Total),
            neighbour.se.Total = sd(Total)/sqrt(length(Total))) %>%
  rename(Pop.neighbour = Pop.target)
str(neighbour.means)

## add means to the rest of the data
new.data <- data %>%
  left_join(pop.means, by = "Pop.target") %>%
  left_join(neighbour.means, by = "Pop.neighbour")
str(new.data)
sum(is.na(new.data)) #no NA values

#### calculate competitive metrics ###
comp.df <- new.data %>%
  mutate(tolerance.Above = log(target.Above/mean.Above),
         tolerance.Below = log(target.Below/mean.Below),
         tolerance.Total = log(target.Total/mean.Total)) %>% # calculate competitive tolerance
  mutate(suppression.Above = log(neighbour.Above/neighbour.mean.Above),
         suppression.Below = log(neighbour.Below/neighbour.mean.Below),
         suppression.Total = log(neighbour.Total/neighbour.mean.Total)) %>% # calculate competitive suppression
  dplyr::select('Pot Number', Pop.target, Target.plant, Pop.neighbour, target.Above, target.Below, target.Total, 
                tolerance.Above, tolerance.Below, tolerance.Total, suppression.Above, suppression.Below, suppression.Total)
str(comp.df)
sum(is.na(comp.df)) #no NA values

#### export competitive metrics file ####
write_rds(comp.df, "output/Competition metrics.rds")

#### make summary table ####
tableS4 <- comp.df %>% 
  dplyr::select(8:13) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(sign = if_else(value > 0, "Count of facilitative interactions", "Count of competitive interactions")) %>%
  count(variable, sign) %>%
  pivot_wider(names_from = sign, values_from = n, values_fill = 0) %>%
  separate(variable, into = c("Competition metric", "Biomass category"), sep = "\\.")
str(tableS4)

#### export table ####
write_csv(tableS4, "output/interaction counts.csv")
