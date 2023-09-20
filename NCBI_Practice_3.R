library(rentrez)

#dir.create("collagen_fasta_sequences") # Run only once

example_list = c('Homo sapiens',
                 'Gallus gallus',
                 'Canis lupus',
                 'Dryobates pubescens',
                 'Ursus arctos',
                 'Oncorhynchus',
                 'Ahaetulla prasina',
                 'Accipitridae',
                 'Balaenoptera acutorostrata',
                 'Panthera pardus')

get_fasta <- function(species_name) {
  if(is.null(species_name)) {
  species_name <- readline("Enter the species name: ")
  }
  
  query <- paste(species_name, "Collagen Type II", sep = " ")
  search_results <- entrez_search(db="nucleotide", term = query)
  
  if (!file.exists("collagen_fasta_sequences")) {
    dir.create("collagen_fasta_sequences")
  }
  
  if(search_results$count > 0) {
  fasta_data <- entrez_fetch(db="nucleotide", id=search_results$ids[1], rettype = "fasta", retmode = "text")
  cat(fasta_data, file = file.path("collagen_fasta_sequences", paste0(species_name, "_Collagen_II.fasta")), sep = " ")
  print(paste("Sucessfully record Collagen Type II FASTA sequence for", species_name, sep = " "))
  } else {
    print(paste("Not found Collagen Type II FASTA sequence for", species_name, sep = " "))
    break
  }
}

get_fasta(NULL)

# Run for all example list
n <- length(example_list)
for(n in example_list) {
  get_fasta(n)
}
