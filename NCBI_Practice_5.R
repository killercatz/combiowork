library(seqinr)
library(read.gb)
library(shiny)

ui <- fluidPage(
  titlePanel("Shiny Text"),
  sidebarLayout(
    sidebarPanel(
      selectInput("data", "Choose a data type:",
                  choices = c("FASTA", "Genbank"),selected = "FASTA"),
      selectInput("species", "Select Species:",
                  choices = c('Homo sapiens',
                              'Gallus gallus',
                              'Canis lupus',
                              'Dryobates pubescens',
                              'Ursus arctos',
                              'Oncorhynchus',
                              'Ahaetulla prasina',
                              'Accipitridae',
                              'Balaenoptera acutorostrata',
                              'Panthera pardus'),
                  selected = "Homo sapiens")
    ),
    mainPanel(
      htmlOutput('data')
    )
  )
)

server <- function(input, output) {
  
  output$data <- renderUI({
    selectfile = paste0("collagen_fasta_sequences/", input$species, "_Collagen_II.fasta")
    
    HTML(paste(gsub("(.{50})", "\\1 \n", toupper(read.fasta(file = selectfile,
                                   seqtype = "DNA", as.string = TRUE)))))
    
  })
  
}

shinyApp(ui = ui, server = server)

read.table(file = "collagen_fasta_sequences/Accipitridae_Collagen_II.fasta")
data <- system.file('collagen_genbank/Canis lupus_Collagen_II.gb')
read.gb(File = 'collagen_genbank/Canis lupus_Collagen_II.gb', DNA = TRUE, Type = "full", Source = "File")
