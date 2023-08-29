library(palmerpenguins)
library(tidyverse)
#mass_flipper <- ggplot(data = penguins, 
#mass_flipper <- ggplot(data = penguins, 
#                       aes(x = bill_depth_mm,
#                           y = bill_length_mm)) +
#  geom_point(aes(color = species, 
#                 shape = species),
#             size = 3,
#             alpha = 0.8) +
#  scale_color_manual(values = c("orange2","purple3","cyan4")) +
#  labs(title = "Penguin size, Palmer Station LTER",
#       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
#       x = "Bill Depth (mm)",
#       y = "Bill length (mm)",
#       color = "Penguin species",
#       shape = "Penguin species") +
#  theme(legend.position = c(0.11, 0.88),
#        plot.title.position = "plot",
#        plot.caption = element_text(hjust = 0, face= "bold"),
#        plot.caption.position = "plot")

#mass_flipper

print("Select the parameter") # There is no way to display all in a single table
print("1. Bill Length")
print("2. Bill Depth")
print("3. Flipper Length")
print("4. Body Mass")

c_para <- as.integer(readline(prompt = "Select choice: "))

if(c_para == 1) {
  parameter <- penguins$bill_length_mm
  lable <- 'Bill Length (mm)'
  table_sub <- "Bill length of Adelie, Chinstrap and Gentoo Penguins"
  return()
}
if(c_para == 2) {
  parameter <- penguins$bill_depth_mm
  lable <- 'Bill Depth (mm)'
  table_sub <- "Bill depth of Adelie, Chinstrap and Gentoo Penguins"
  return()
}
if(c_para == 3) {
  parameter <-penguins$ flipper_length_mm
  lable <- 'Flipper Length (mm)'
  table_sub <- "Flipper length of Adelie, Chinstrap and Gentoo Penguins"
  return()
}
if(c_para == 4) {
  parameter <- penguins$body_mass_g
  lable <- 'Body Mass (g)'
  table_sub <- "Body mass of Adelie, Chinstrap and Gentoo Penguins"
  return()
}
if(c_para < 1 || c_para > 4) {
  stop("The value is not valid. Please try again") # Assign value for graph
  # Will stop if value is not 1-4
}

ggplot(data = penguins, aes(x = species, y = parameter)) + # Plotting
  geom_jitter(aes(color = species),
              width = 0.1, 
              alpha = 0.7,
              show.legend = FALSE) +
            labs(title = "Penguin from Palmerpenguins", # Assign lable
            subtitle = table_sub,
            y = lable,
            x = "Species") +
  scale_color_manual(values = c("red3","blue","cyan4")) # Assign color
