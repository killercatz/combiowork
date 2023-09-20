library(rentrez)

#dir.create("collagen_genbank") # Run only once

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

get_genbank <- function(species_name) {
  if(is.null(species_name)) {
    species_name <- readline("Enter the species name: ")
  }
  
  query <- paste(species_name, "Collagen Type II", sep = " ")
  search_results <- entrez_search(db="protein", term = query)
  
  if (!file.exists("collagen_genbank")) {
    dir.create("collagen_genbank")
  }
  
  if(search_results$count > 0) {
    fasta_data <- entrez_fetch(db="protein", id=search_results$ids[1], rettype = "fasta", retmode = "text")
    cat(fasta_data, file = file.path("collagen_genbank", paste0(species_name, "_Collagen_II.gb")), sep = " ")
    print(paste("Sucessfully record Collagen Type II genbank for", species_name, sep = " "))
  } else {
    print(paste("Not found Collagen Type II genbank for", species_name, sep = " "))
    break
  }
}

get_fasta(NULL)

# Run for all example list
n <- length(example_list)
for(n in example_list) {
  get_genbank(n)
}
