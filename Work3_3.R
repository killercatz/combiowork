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

for (col in selected_columns) {
  
  anova_result <- aov(as.formula(paste(col, " ~ species")), data = penguins)

  print(tidy(anova_result))
  
}

#Q4: Please find pairwise relationship between bill length, bill depth,
#flipper length, and body mass. Are these relationship the same or different between
#the three penguin species?

# Text Version
for (i in 1:3) {
  
  target_species <- switch(i, "Adelie", "Gentoo", "Chinstrap")
  
  select_penguin <- as.matrix(na.omit(penguins  %>% filter(species == target_species) 
                                      %>% select(bill_length_mm,
                                                            bill_depth_mm,
                                                            flipper_length_mm,
                                                            body_mass_g)))

  corr <- cor(select_penguin)
  cat('The correlation for', target_species, 'is\n')
  print(corr)
}

# Graph Version
pairs(as.matrix(na.omit(penguins %>% select(bill_length_mm,
                                   bill_depth_mm,
                                   flipper_length_mm,
                                   body_mass_g))),
      main = "Correlation of penguin species",
      gap = 1,
      font.lables = 1,
      pch = 21,
      bg = c("red3","blue","cyan4"),
      col = c("red3","blue","cyan4"),
      lower.panel = panel.smooth,
      upper.panel = NULL)

