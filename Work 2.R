#default

print("1.1 What is melting temperature of DNA mmolecules that is 450 bp long and has 50% GC content?")

Na <- 100
GC <- 50
length <- 450
melting_temp <- 81.5 + (16.6 * log10(Na)) + (0.14 * GC) - (500 / length)
cat("Ans: The melting tempurature is ", melting_temp, "°C\n")

print("1.2 What is % GC content oof a DNA molecule 800 bp long and have melting temperature of 92°C")

length <- 800
melting_temp <- 92
#GC% = (Tm-(81.5+16.6 * log10([NA]))+(500/length)/0.41
GC <- (melting_temp-(81.5+16.6 * log10(Na)))+(500/length)/0.41
cat("Ans: The % GC content of this DNA is ", GC, "%\n")

print("1.3 What is max melting °C of DNA sample that is 500 bp long?")

length <- 500
GC <- 100 #Since GC not indicate the maximum will be 100%
melting_temp <- 81.5 + (16.6 * log10(Na)) + (0.14 * GC) - (500 / length)
cat("Ans: The maximum melting tempurature is ", melting_temp, "°C\n")

#print("2.1 Suppose that a drug has a removal rate constant of r = 0.080 1/hr and an initial
#concentration (C0) of 5.0 mg/L. Find the drug concentration (mg/L) after 4.0 hours.")

#Co <- 5
#r <- 0.080
#C <- Co *


