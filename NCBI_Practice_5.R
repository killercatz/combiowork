library(seqinr)
library(shiny)

ui <- fluidPage(
  titlePanel("FASTA and Genbank data display"),
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
      htmlOutput('data'),
      textOutput('gb')
    )
  )
)

server <- function(input, output) {
  
  output$data <- renderUI({
    if(input$data == 'FASTA') {
    selectfile = paste0("collagen_fasta_sequences/", input$species, "_Collagen_II.fasta")
    
    HTML(paste(gsub("(.{90})", "\\1 \n", toupper(read.fasta(file = selectfile,
                                   seqtype = "DNA", as.string = TRUE)))))
    }
    
    else {
    }
    
    
  })
  output$gb <- renderText({
      selectfile = (paste0("collagen_genbank/", input$species, "_Collagen_II.gb"))
      
      paste0(gsub("(.{90})", "\\1 \n",readLines(selectfile)))
    
  })
  
}

shinyApp(ui = ui, server = server)
