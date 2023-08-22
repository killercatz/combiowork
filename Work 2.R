#default
# Q1

#install.packages('SciViews') # Do for first time
library("SciViews") # For ln() function

cat("1.1 What is melting temperature of DNA mmolecules that is 450 bp long and has 50% GC content?")

default <- 1
negative = default * -1
Na <- 0.1 # 100 mM = 0.1 Mole
GC <- 50
length <- 450
melting_temp <- 81.5 + (16.6 * log10(Na)) + (0.41 * GC) - (500 / length)
cat("\nAns: The melting tempurature is ", round(melting_temp, digit = 2), "°C\n")

cat("1.2 What is % GC content oof a DNA molecule 800 bp long and have melting temperature of 92°C")

length <- 800
melting_temp <- 92
#GC% = (Tm-(81.5+16.6 * log10([NA]))+(500/length)/0.41
GC <- (melting_temp-(81.5+16.6 * log10(Na)))+(500/length)/0.41
cat("\nAns: The % GC content of this DNA is ", round(GC, digit = 2), "%\n")

cat("1.3 What is max melting °C of DNA sample that is 500 bp long?")

length <- 500
GC <- 100 #Since GC not indicate the maximum will be 100%
melting_temp <- 81.5 + (16.6 * log10(Na)) + (0.41 * GC) - (500 / length)
cat("\nAns: The maximum melting tempurature is ", round(melting_temp, digit = 2), "°C\n")

# Q2

cat("2.1 Suppose that a drug has a removal rate constant of r = 0.080 1/hr and an initial
concentration (C0) of 5.0 mg/L. Find the drug concentration (mg/L) after 4.0 hours.")

Co <- 5
r <- 0.080
t <- 4
e <- 2.71828
drugC <- Co * (e ^ (r * t * negative))

cat("\nAns: The drug concentration after 4 hours is ", round(drugC, digit = 2), "°mg/l\n")

cat("2.2 If for Drug X, r = 0.20 1/hr, how long after injection does it take for the
concentration of a Drug X to decrease to 35% of its initial level?")

r <- 0.20
drugC <- 35 / 100
var1 <- 0.2 * -1
var2 <- ln(drugC)
t <- solve(var1, var2) # ln(0.35) = -0.2t

cat("\nAns: The time take for the concentration of a Drug X to decrease ", round(t, digit = 2), "°hr/l\n")

cat("2.3 Suppose Drug X is ineffective when the concentration drops below 0.50 mg/L.\nIf r = 0.14 1/hr, what concentration must be initiated \nIf the concentration is to be 0.50 mg/L after 6.0 hours?")

r <- 0.14
t <- 6
drugC <- 0.50
var1 <- r * -1

Co <- 0.5 / (e ^ (var1 * t))

cat("\nAns: The initial concentration is ", round(Co, digit = 2), "°mg/l\n")

cat("2.4 Suppose a drug is administered with an initial concentration of 2.60 mg/L.\nIf r = 0.120 for this drug, and the drug is ineffective when the concentration falls below 0.650 mg/L, \nwhat is the approximate maximum time a nurse can wait to administer another dose?\n")

Co <- 2.6
r <- 0.120
drugC <- 0.650 # 0.65 = 2.6e^(-0.2*t)
var1 <- ln(drugC / Co)
var2 <- r * -1
t <- solve(var2, var1)

cat("Ans: The maximum time nurse can wait is ", round(t, digit = 2), "°hr\n")

cat("2.5 If r = 0.22 1/hr for a particular drug, how long does it take for the concentration to be half the initial concentration?") 

r <- 0.22 # C = 0.5 * Co * e ^ (0.22 * t)
var1 <- ln(0.5)
t = solve(r, var1)

cat("\nAns: It take ", abs(round(t, digit = 2)), " hr for concentration to be half\n")

# Q3

#This function is to check BMI level
bmicheck <- function(x){
  if(x <= 18.5){
    BMI = "Underweight"
  } else {
    if(x > 18.5 && x <= 24.9){
      BMI = "Normal"
    } else {
      if(x > 24.9 && x <= 29.9){
        BMI = "Overweight"
      } else {
        if(x > 29.9){
          BMI = "Obese"
        }
      }}}
  cat(BMI)
}

cat("3.1 What is the approximate BMI for a 150 lb person that is 5' 10' tall?\n")

weight = 150
height = 70 # 5' 10" = 70 inch
BMIscore = (703 * weight) / (height ^ 2)

cat("Ans: BMI of 150 lb and 5' 10' is ")
bmicheck(BMIscore)

cat("\n3.2 If a person is categorized as obese at 225 pounds, what is the maximum height they could be?\n")

weight = 255
height = sqrt((703 * weight) / 30) # 30 is the minimum BMI score for obese

cat("Ans: The maximum hieght for 255lb person to classify as obese is ", height, "inchs\n")

cat("3.3 Suppose a person with weight W pounds and height H inches has a BMI of C.
What is the BMI of a person with weight W and height 4/5H?\n")

var1 <- 5^2 # 4/5 become 5/4 when switching side and ^2 from [height ^ 2]
var2 <- 4^2 # 25/16 is new modifier for BMI formular
cat("Ans: The new BMI formular will be ", (var1/var2) ,"* (703 * weight) / (height *2)\n")
# 1.5625 is modifier for 4/5 of height for BMI formular

# Q4

cat("4.1 How many bacteria are present after 51 hours if a culture is inoculated with 1 bacterium?\n")

t <- 51
No <- 1
k = (ln(2)/3) # 2 is population double, 3 is every 3 hours
N = No * (e ^ (k*t))

cat("Ans: After 51 hours incubation there will be ", round(N, digit = 2) ," bacteria\n")

cat("4.2 With how many bacteria should a culture be inoculated if there are to be 81,920 bacteria present on hour 42? Assume the population doubles every 3 hours.")

t <- 42
N <- 81920
No <- 81920 / (e ^ (k*t))

cat("Ans: The inoculation should start with ", round(No, digit = 0) ," bacteria\n")
