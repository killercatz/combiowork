library(tidyverse) # For ggplot.
library(gridExtra) # For tables.
library(dplyr) # For data extraction.
library(DescTools) # For mode calculation.
library(palmerpenguins)

data("penguins")

#Q1: Please find the minimum, maximum, mean, mode, median,
#SD, range, and confidence interval of bill_length, bill depth,
#flipper length, and body mass of each penguin species.

selected_columns <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
i <- 1

for (col in selected_columns) {
  
  target_species <- switch(i, "Adelie", "Gentoo", "Chinstrap")
  
  bill_lengths_species <- na.omit(penguins %>% filter(species == target_species) 
                                  %>% select(bill_length_mm))
  bill_depth_species <- na.omit(penguins %>% filter(species == target_species) 
                                %>% select(bill_depth_mm))
  flipper_lengths_species <- na.omit(penguins %>% filter(species == target_species) 
                                     %>% select(flipper_length_mm))
  body_mass_species <- na.omit(penguins %>% filter(species == target_species) 
                               %>% select(body_mass_g))
  
  for (j in 1:4) {
    
    target_parameter_name <- switch(j, "Bill Length", "Bill Depth", "Flipper Length", "Body Mass")
    if(j == 1) {
      target_parameter <- bill_lengths_species
    }
    if(j == 2) {
      target_parameter <- bill_depth_species
    }
    if(j == 3) {
      target_parameter <- flipper_lengths_species
    }
    if(j == 4) {
      target_parameter <- body_mass_species
    }
    
    cat('The analysis for ', target_species, ' penguin is:\n')
    cat(target_parameter_name, '>>>\n')
    cat('Min: ', sapply(target_parameter, min), ' ||| ')
    cat('Max: ', sapply(target_parameter, max), '\n')
    cat('Mean: ', round(sapply(target_parameter, mean), 2), ' ||| ')
    cat('Mode: ', sapply(target_parameter, Mode), '\n')
    cat('SD: ', round(sapply(target_parameter, sd), 2), ' ||| ')
    cat('Range: ', range(target_parameter, na.rm = FALSE), '\n')
    
    n <- sapply(target_parameter, length)
    standard_error <- sapply(target_parameter, sd) / sqrt(n)
    degreesf <- n - 1
    t_score <- qt(p=0.05/2, df=degreesf,lower.tail=F)
    margin_error <- t_score * standard_error
    lower_bound <- sapply(target_parameter, mean) - margin_error
    upper_bound <- sapply(target_parameter, mean) + margin_error
    cat('Confidence Interval: ', round(lower_bound, 2), round(upper_bound, 2), '\n\n')
  }

  i <- i + 1
  
  if(i == 4) {
    break
  }
}

#print("1. Adelie") # This one have all black head
#print("2. Gentoo") # This one have white eyebrow
#print("3. Chinstrap") # This one have black line on its chin
