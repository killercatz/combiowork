print("Select choice of operation")
print("1. Calculate area of circle")
print("2. Calculate volume of box")

choice <-as.integer(readline(prompt = "Enter your choice: "))

if(choice == 1) {
  radius <-as.integer(readline(prompt = "Define circle radius: "))
  
  cat("The area of circle is: " , 3.14 * radius^2)
} else {
  if(choice == 2){
    length <-as.integer(readline(prompt = "Define length: "))
    width <-as.integer(readline(prompt = "Define width: "))
    height <-as.integer(readline(prompt = "Define height: "))
    
    cat("The volume of box is: " , length * width * height)
  } else {
  print("Please select valid choice")
}}

  