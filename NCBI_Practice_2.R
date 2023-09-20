library(rentrez)
library(XML)

query <- "COVID-19"

search_results <- entrez_search(db = "SRA", term = query)
search_summary <- entrez_summary(db= "SRA", id=search_results$ids[1], rettype="xml", parsed=TRUE)
fetch_xml <- entrez_fetch(db = "sra", id = search_results$ids[1], rettype = "xml")

fetch_parsed <- xmlParse(fetch_xml)

uid <- xpathSApply(fetch_parsed, "//PRIMARY_ID", xmlValue)
first_name <- xpathSApply(fetch_parsed, "//First", xmlValue)
last_name <- xpathSApply(fetch_parsed, "//Last", xmlValue)

cat("The latest viral genome of COVID-19 upload to NCBI is:", uid[1], "\n")
cat("Upload date:", search_summary$createdate, "\n")
cat("The author(s):\n")

for (col in 1:5) {
  full_name <- paste(first_name[col], last_name[col], sep = " ")
  print(full_name)
}
