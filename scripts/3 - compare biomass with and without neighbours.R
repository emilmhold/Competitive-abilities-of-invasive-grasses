## Compare biomass for plants grown alone and with neighbours
## Author: Emily H
## Created: April 21, 2025
## Last edited: October 9, 2025

#install.packages("tidyverse")

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(nlme)
library(emmeans)

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
            se.Total = sd(Total)/sqrt(length(Total))) %>% ##summaries for log-transformed data
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
            se.Total = sd(target.Total)/sqrt(length(target.Total))) %>% ##summaries for log-transformed data
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

#### models ####
## reformat neighbour and control data to merge
target.data <- data %>%
  dplyr::select(!c(`Pot Number`, Target.plant, Pop.neighbour, neighbour.Above, neighbour.Below, neighbour.Total)) %>%
  rename(Above = target.Above,
         Below = target.Below,
         Total = target.Total) %>%
  mutate(Species = case_when(
    startsWith(Pop.target, "Br") ~ "B. inermis",
    startsWith(Pop.target, "Po") ~ "P. pratensis",
    startsWith(Pop.target, "Ag") ~ "A. cristatum")) %>% #create new column for species identity
  mutate(Neighbours = "with neighbour", .before = Above)
str(target.data)

controls.to.merge <- controls %>%
  mutate(Species = case_when(
    startsWith(Pop.target, "Br") ~ "B. inermis",
    startsWith(Pop.target, "Po") ~ "P. pratensis",
    startsWith(Pop.target, "Ag") ~ "A. cristatum")) %>% #create new column for species identity
  mutate(Neighbours = "alone", .before = Above)
str(controls.to.merge)

### merge dataframes
data.for.models <- rbind(target.data, controls.to.merge)
str(data.for.models)

#### Aboveground biomass ####
#linear mixed model
lme.ab.neighbour.biomass <- lme(Above ~ Neighbours*Species, random = ~ 1|Pop.target, data = data.for.models)
summary(lme.ab.neighbour.biomass)
anova(lme.ab.neighbour.biomass)

#post hoc tests
emmeans(lme.ab.neighbour.biomass, list(pairwise ~ Species:Neighbours), adjust = "tukey", data = data.for.models)

#Residual normality test
resid <- residuals(lme.ab.neighbour.biomass)
shapiro.test(resid)
hist(resid)

#### Belowground biomass ####
#linear mixed model
lme.bg.neighbour.biomass <- lme(Below ~ Neighbours*Species, random = ~ 1|Pop.target, data = data.for.models)
summary(lme.bg.neighbour.biomass)
anova(lme.bg.neighbour.biomass)

#post hoc tests
emmeans(lme.bg.neighbour.biomass, list(pairwise ~ Species:Neighbours), adjust = "tukey", data = data.for.models)

#Residual normality test
resid <- residuals(lme.bg.neighbour.biomass)
shapiro.test(resid)
hist(resid)
plot(resid)

#### Total biomass ####
#linear mixed model
lme.total.neighbour.biomass <- lme(Total ~ Neighbours*Species, random = ~ 1|Pop.target, data = data.for.models)
summary(lme.total.neighbour.biomass)
anova(lme.total.neighbour.biomass)

#post hoc tests
emmeans(lme.total.neighbour.biomass, list(pairwise ~ Species:Neighbours), adjust = "tukey", data = data.for.models)

#Residual normality test
resid <- residuals(lme.total.neighbour.biomass)
shapiro.test(resid)
hist(resid)
plot(resid)

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
  annotate("text",x=1,y=0.7,label="***",cex=5)+ 
  annotate("text",x=2,y=0.7,label="***",cex=5)+
  annotate("text",x=3,y=0.7,label="***",cex=5)+
  ylim(0,1.7)+
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
  annotate("text",x=1,y=1.2,label="ns",cex=5)+ 
  annotate("text",x=2,y=1.2,label="*",cex=5)+
  annotate("text",x=3,y=1.2,label="***",cex=5)+
  ylim(0, 1.7)+
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
  annotate("text",x=1,y=1.2,label="ns",cex=5)+ 
  annotate("text",x=2,y=1.2,label="***",cex=5)+
  annotate("text",x=3,y=1.6,label="***",cex=5)+
  ylim(0, 1.7)+  
  ylab(" ")
total.biomass.comparison

#### put  plots together ####
biomass.legend <- cowplot::get_legend(total.biomass.comparison)
neighbour.biomass.plots <- cowplot::plot_grid(aboveground.biomass.comparison + theme(legend.position = "none"), 
                                              belowground.biomass.comparison + theme(legend.position = "none"), 
                                              total.biomass.comparison + theme(legend.position = "none"),
                                              labels = "auto",
                                              label_size = 20,
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

#### make results tables ####
#### anova tables ####
# Define the row labels you want
model_list <- list(
  lme.ab.neighbour.biomass = lme.ab.neighbour.biomass,
  lme.bg.neighbour.biomass = lme.bg.neighbour.biomass,
  lme.total.neighbour.biomass = lme.total.neighbour.biomass
)

desired_labels <- list(
  'Aboveground biomass', 
  'Belowground biomass',
  'Total biomass'
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
write_csv(results, "output/alone_biomass_anova_results.csv")

#### post-hoc test tables ####
## write function to extract pairwise comparisons
extract_posthoc_info <- function(model, model_name, data) {
  # Run emmeans pairwise comparisons
  emmeans_result <- emmeans(model, pairwise ~ Species:Neighbours, adjust = "tukey", data = data.for.models)
  
  
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
posthoc_results <- map2_dfr(model_list, names(model_list), ~ extract_posthoc_info(.x, .y, data = data.for.models))

# filter the results to include only within-species comparisons
filtered_results <- posthoc_results %>%
  filter(str_extract(Contrast, "^[^ ]+") == str_extract(Contrast, "(?<= - )[^ ]+"))

# Export to CSV
write_csv(filtered_results, "output/alone_biomass_posthoc_results.csv")
