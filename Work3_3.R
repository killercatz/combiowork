library(tidyverse) # For ggplot.
library(gridExtra) # For tables.
library(dplyr) # For data extraction.
library(DescTools) # For mode calculation.
library(broom) # Tidy function
library(palmerpenguins)

#data("penguins")

#Q3: How to statistically test whether these three penguin species 
#have the same or different bill length, bill depth, flipper length, and body mass?

selected_columns <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
  
select_penguin <- data.frame(na.omit(penguins %>% filter(species == 'Adelie') 
                          %>% select(bill_length_mm)))
anova_result <- aov(select_penguin ~ species, data = penguins)

anova_summary <- tidy(anova_result)
print(anova_summary)

#Q4: Please find pairwise relationship between bill length, bill depth,
#flipper length, and body mass. Are these relationship the same or different between
#the three penguin species?
  
par(mfrow = c(2, 2))

  boxplot(bill_length_mm ~ species, data = penguins)
  boxplot(bill_depth_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(body_mass_g ~ species, data = penguins)