library(tidyverse) # For ggplot.
library(gridExtra) # For tables.
library(dplyr) # For data extraction.
library(DescTools) # For mode calculation.
library(palmerpenguins)

#Q1: Please find the minimum, maximum, mean, mode, median,
#SD, range, and confidence interval of bill_length, bill depth,
#flipper length, and body mass of each penguin species.

print("Select penguin species") # There is no way to display all species in a single table
print("1. Adelie") # This one have all black head
print("2. Gentoo") # This one have white eyebrow
print("3. Chinstrap") # This one have black line on its chin

c_species <- as.integer(readline(prompt = "Select choice: "))

if(c_species == 1) {
  Name <- 'Adelie'
  pre_select_penguin <- penguins %>% filter(species == "Adelie") # Select species
  select_penguin <- na.omit(pre_select_penguin) # Remove NA
  ### Omit will remove all data with NA in any position
} else {
  if(c_species == 2) {
    Name <- 'Gentoo'
    pre_select_penguin <- penguins %>% filter(species == "Gentoo")
    select_penguin <- na.omit(pre_select_penguin)
  } else {
    if(c_species == 3) {
      Name <- 'Chinstrap'
      pre_select_penguin <- penguins %>% filter(species == "Chinstrap")
      select_penguin <- na.omit(pre_select_penguin)
    } else {
      stop("The value is not valid. Please try again")
    }}}

#Bill Length
cat('The analysis for ', Name, ' penguin is:\n')
cat('Bill Length:\n')
cat('Min: ', min(select_penguin$bill_length_mm), '\n')
cat('Max: ', max(select_penguin$bill_length_mm), '\n')
cat('Mean: ', round(mean(select_penguin$bill_length_mm), digit = 2), '\n')
var1 <- Mode(select_penguin$bill_length_mm)
cat('Mode: ', var1, '\n')
cat('SD: ', round(sd(select_penguin$bill_length_mm), digit = 2), '\n')
cat('Range: ', range(select_penguin$bill_length_mm, na.rm = FALSE), '\n')

n <- length(select_penguin$species)
standard_error <- sd(select_penguin$bill_length_mm) / sqrt(n)
degreesf <- n - 1
t_score <- qt(p=0.05/2, df=degreesf,lower.tail=F)
margin_error <- t_score * standard_error
lower_bound <- mean(select_penguin$bill_length_mm) - margin_error
upper_bound <- mean(select_penguin$bill_length_mm) + margin_error
cat('Confidence Interval: ', round(lower_bound, digit = 2), round(upper_bound, digit = 2), '\n')
#Bill Depth
cat('\nBill Depth:\n')
cat('Min: ', min(select_penguin$bill_depth_mm), '\n')
cat('Max: ', max(select_penguin$bill_depth_mm), '\n')
cat('Mean: ', round(mean(select_penguin$bill_depth_mm), digit = 2), '\n')
var1 <- Mode(select_penguin$bill_depth_mm)
cat('Mode: ', var1, '\n')
cat('SD: ', round(sd(select_penguin$bill_depth_mm), digit = 2), '\n')
cat('Range: ', range(select_penguin$bill_depth_mm, na.rm = FALSE), '\n')

standard_error <- sd(select_penguin$bill_depth_mm) / sqrt(n)
margin_error <- t_score * standard_error
lower_bound <- mean(select_penguin$bill_depth_mm) - margin_error
upper_bound <- mean(select_penguin$bill_depth_mm) + margin_error
cat('Confidence Interval: ', round(lower_bound, digit = 2), round(upper_bound, digit = 2), '\n')
#Flipper Length
cat('\nFlipper Length:\n')
cat('Min: ', min(select_penguin$flipper_length_mm), '\n')
cat('Max: ', max(select_penguin$flipper_length_mm), '\n')
cat('Mean: ', round(mean(select_penguin$flipper_length_mm), digit = 2), '\n')
var1 <- Mode(select_penguin$flipper_length_mm)
cat('Mode: ', var1, '\n')
cat('SD: ', round(sd(select_penguin$flipper_length_mm), digit = 2), '\n')
cat('Range: ', range(select_penguin$flipper_length_mm, na.rm = FALSE), '\n')

n <- length(select_penguin$species)
standard_error <- sd(select_penguin$flipper_length_mm) / sqrt(n)
degreesf <- n - 1
t_score <- qt(p=0.05/2, df=degreesf,lower.tail=F)
margin_error <- t_score * standard_error
lower_bound <- mean(select_penguin$flipper_length_mm) - margin_error
upper_bound <- mean(select_penguin$flipper_length_mm) + margin_error
cat('Confidence Interval: ', round(lower_bound, digit = 2), round(upper_bound, digit = 2), '\n')
#Body Mass
cat('\nBody Mass:\n')
cat('Min: ', min(select_penguin$body_mass_g), '\n')
cat('Max: ', max(select_penguin$body_mass_g), '\n')
cat('Mean: ', round(mean(select_penguin$body_mass_g), digit = 2), '\n')
var1 <- Mode(select_penguin$body_mass_g)
cat('Mode: ', var1, '\n')
cat('SD: ', round(sd(select_penguin$body_mass_g), digit = 2), '\n')
cat('Range: ', range(select_penguin$body_mass_g, na.rm = FALSE), '\n')

n <- length(select_penguin$species)
standard_error <- sd(select_penguin$body_mass_g) / sqrt(n)
degreesf <- n - 1
t_score <- qt(p=0.05/2, df=degreesf,lower.tail=F)
margin_error <- t_score * standard_error
lower_bound <- mean(select_penguin$body_mass_g) - margin_error
upper_bound <- mean(select_penguin$body_mass_g) + margin_error
cat('Confidence Interval: ', round(lower_bound, digit = 2), round(upper_bound, digit = 2), '\n')

#  bill_length_mm <- select_penguin$bill_length_mm
#  bill_depth_mm <- select_penguin$bill_depth_mm
#  flipper_length_mm <- select_penguin$flipper_length_mm
#  body_mass_g <- select_penguin$body_mass_g  

#data <- data.frame(Entries = c('Bill Length','Bill Depth', 'Flipper Length', 'Body Mass'),
#                   Min = c(min(bill_length_mm),min(bill_depth_mm),min(flipper_length_mm),min(body_mass_g)),
#                   Max = c(max(bill_length_mm),max(bill_depth_mm),max(flipper_length_mm),max(body_mass_g)),
#                   Mean = c(mean(bill_length_mm),mean(bill_depth_mm),mean(flipper_length_mm),mean(body_mass_g)))

# This one take me 5 hours, it must be hardest
# Data visualization is unreal for me rn.
