library(dplyr)

data <- read.csv("examdata.csv")
primer <- c('G7', 'G9', 'G21', 'G22', 'G23', 'G24', 'G25', 'G26', 'G27', 'G28', 'G29',
           'G30', 'G31', 'G32', 'G33', 'G35', 'G36', 'G40', 'G42')

coll <-  data['1',] %>% select(Sample)

wordremove <- paste(primer, collapse = "|")
output <- gsub(wordremove, "", coll)

totalrow <- nrow(data)

for(number in 1:totalrow) {
  if(number == 1) {
    print("Processing data. Please wait.")
    defaultvalue <- ''
    sample_name <- NULL
  } 
  
  coll <-  data[number,] %>% select(Sample)
  
  output <- gsub(wordremove, "", coll)
  
  if(output == defaultvalue) {} else {
    defaultvalue <- output
    
    if(output %in% sample_name) {} else {
      sample_name <- append(sample_name, output)
    }
  }
  
  if(number == totalrow) {
    unique_name <- unique(sample_name)
    print("Completed.")
  }
}

