library(dplyr)

data <- read.csv("examdata.csv")
data <- rbind(data, data['1',])
primer <- c('G7', 'G9', 'G21', 'G22', 'G23', 'G24', 'G25', 'G26', 'G27', 'G28', 'G29',
            'G30', 'G31', 'G32', 'G33', 'G35', 'G36', 'G40', 'G42')

#coll <-  data['1',] %>% select(Sample)
#wordremove <- paste(primer, collapse = "|")
#output <- gsub(wordremove, "", coll)

totalrow <- nrow(data)

progressbar <- txtProgressBar(min = 0,
                              max = totalrow,
                              style = 3,
                              width = 50,
                              char = "▄")

for(number in 1:totalrow) {
  if(number == 1) {
    print("Processing data. Please wait.")
    defaultvalue <- 'MW1'
    sample_name <- NULL
    
    appendframe <- data.frame(
      No = c(''), Primer = c(""), Sample_name = c(''), Band1 = c(''), Band2 = c(''),
      Band3 = c(''), Band4 = c(''), Band5 = c(''), Band6 = c(''), Band7 = c(''),
      Band8 = c(''), Band9 = c(''), Band10 = c(''), Band11 = c(''), Band12 = c(''), 
      Band13 = c(''), Band14 = c(''), Band15 = c('')
    )
    
    appendframe <- appendframe[-1,]
    
    for(x in 1:15) {
      Band <- paste('b', x, sep = '')
      assign(Band, 0)
    }
    
    n <- 1
  }
  
  coll <- data[number,] %>% select(Sample)
  wordremove <- paste(primer, collapse = "|")
  output <- gsub(wordremove, "", coll)
  
  setTxtProgressBar(progressbar, number)
  
  if(output == defaultvalue) {} else {
    
    if(number > 1) { oldnumber <- number - 1 }

    newframe <- data.frame(
      No = c(n), Primer = c(data[oldnumber,] %>% select(Primer)),
      Sample_name = c(defaultvalue), Band1 = c(b1), Band2 = c(b2),
      Band3 = c(b3), Band4 = c(b4), Band5 = c(b5), Band6 = c(b6), Band7 = c(b7),
      Band8 = c(b8), Band9 = c(b9), Band10 = c(b10), Band11 = c(b11), 
      Band12 = c(b12), Band13 = c(b13), Band14 = c(b14), Band15 = c(b15)
    )
      
    n <- n + 1
      
    for(x in 1:15) {
      Band <- paste('b', x, sep = '')
      assign(Band, 0)
    }
  
    appendframe <- rbind(appendframe, newframe)
    defaultvalue <- output
  }
  
  bandno <- data[number,] %>% select(Band)
  molecular_w <- as.numeric(data[number,] %>% select(Molecular.Weight))
  
  Band <- paste('b', bandno, sep = '')
  assign(Band, molecular_w)
  
  if(number == totalrow) {
    write.csv(appendframe, "formatted_exam_data.csv", row.names=FALSE)
    cat(" Completed.\n")
    print("Data processed!")
    print("File save as 'formatted_exam_data.csv' ")
    print("Here's short version of modified data: ")
    print(head(appendframe, 20))
    close(progressbar)
  }
}

# Remark: Some data have different lanes but lane does not appear in require format
# I assume that you are not include it hence I didn't code it for each different lane
# The highest lanes appear in formatted version
