library(rentrez)

query <- "Homo[Organism] AND alkaline phosphatase[Gene/Protein Name] NOT 92309"

search_results <- entrez_search(db = "gene", term = query)

cat("Number of alkaline phosphatase genes in Homo sapiens:", search_results$count, "\n")

