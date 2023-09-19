library(shiny)
library(palmerpenguins)
library(plotly)
library(fmsb)

categorize_flipper_length <- function(length) {
  ifelse(length >= 160 & length < 180, "160-179 mm",
         ifelse(length >= 180 & length < 200, "180-199 mm",
                ifelse(length >= 200 & length < 220, "200-219 mm", 
                       ifelse(length >=220 & length < 240,"220-239 mm","Other"))))
}

categorize_bill_depth <- function(depth) {
  ifelse(depth < 15, "Shallow (1-14 mm)",
         ifelse(depth >= 15 & depth < 20, "Medium (15-19 mm)", 
                ifelse(depth >=20 & depth <30,"Deep (>=20 mm)")))
}

categorize_bill_length <- function(length) {
  ifelse(length < 35, "Short (1-34 mm)",
         ifelse(length >= 35 & length < 45, "Medium (35-44 mm)", 
                ifelse(length >= 45 & length < 60,"Long (45-60 mm)")))
}

minimal_colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")

ui <- fluidPage(
  titlePanel("Penguins Dataset Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("chartType", "Select Chart Type:",
                  choices = c("Species Distribution", "Sex Ratio", 
                              "Flipper Length Ranges", "Bill Depth Categories", "Bill Length Categories","Flipper Length 160-179 mm",
                              "Flipper Length 180-199 mm","Flipper Length 200-219 mm","Flipper Length 220-239 mm","Bill Depth 1-14 mm",
                              "Bill Depth 15-19 mm","Bill Depth <= 20 mm","Bill Length 1-34 mm","Bill Length 34-44 mm" ,"Bill Length 45-60 mm"),
                  selected = "Species Distribution"),
      sliderInput("penid", "Specific Penguins", value = 0, min = 0, max = 125)
    ),
    mainPanel(
      plotlyOutput("pieChart", height = "600px"),
      plotOutput("pairsPlot", height = "600px"),
      plotOutput("individualplot"),
      textOutput("individualsex"),
      textOutput("individualyear"),
      textOutput("individualisland"),
      textOutput("individualspecies")
    )
  )
)

server <- function(input, output) {
  
  output$pieChart <- renderPlotly({
    if (input$chartType == "Species Distribution") {
      species_counts <- table(penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Species Distribution")
      
    } else if (input$chartType == "Sex Ratio") {
      sex_counts <- table(penguins$sex)
      
      pie_chart <- plot_ly(labels = names(sex_counts), values = sex_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Sex Ratio")
    } else if (input$chartType == "Diet Composition") {
      diet_counts <- table(penguins$diet)
      
      pie_chart <- plot_ly(labels = names(diet_counts), values = diet_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Diet Composition")
    } else if (input$chartType == "Flipper Length Ranges") {
      penguins$flipper_length_category <- sapply(penguins$flipper_length_mm, categorize_flipper_length)
      flipper_length_counts <- table(penguins$flipper_length_category)
      
      pie_chart <- plot_ly(labels = names(flipper_length_counts), values = flipper_length_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Flipper Length Ranges")
    } else if (input$chartType == "Bill Depth Categories") {
      penguins$bill_depth_category <- sapply(penguins$bill_depth_mm, categorize_bill_depth)
      bill_depth_counts <- table(penguins$bill_depth_category)
      
      pie_chart <- plot_ly(labels = names(bill_depth_counts), values = bill_depth_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Bill Depth Categories")
    } else if (input$chartType == "Bill Length Categories") {
      penguins$bill_length_category <- sapply(penguins$bill_length_mm, categorize_bill_length)
      bill_length_counts <- table(penguins$bill_length_category)
      
      pie_chart <- plot_ly(labels = names(bill_length_counts), values = bill_length_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Bill Length Categories")
    } else if (input$chartType == "Flipper Length 160-179 mm") {
      filtered_penguins <- subset(penguins, flipper_length_mm >= 160 & flipper_length_mm < 180)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Flipper Length (160-179 mm) by Species")
    } else if (input$chartType == "Flipper Length 180-199 mm") {
      filtered_penguins <- subset(penguins, flipper_length_mm >= 180 & flipper_length_mm < 200)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Flipper Length (180-199 mm) by Species")
    } else if (input$chartType == "Flipper Length 200-219 mm") {
      filtered_penguins <- subset(penguins, flipper_length_mm >= 200 & flipper_length_mm < 220)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Flipper Length (200-219 mm) by Species")
    } else if (input$chartType == "Flipper Length 220-239 mm") {
      filtered_penguins <- subset(penguins, flipper_length_mm >= 220 & flipper_length_mm < 240)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Flipper Length (220-239 mm) by Species")      
    } else if (input$chartType == "Bill Depth 1-14 mm") {
      filtered_penguins <- subset(penguins, bill_depth_mm >= 1 & bill_depth_mm < 15)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Bill Depth (1-14 mm) (Shallow) by Species")  
    } else if (input$chartType == "Bill Depth 15-19 mm") {
      filtered_penguins <- subset(penguins, bill_depth_mm >= 15 & bill_depth_mm < 20)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Bill Depth (15-19 mm) (Medium) by Species")        
    } else if (input$chartType == "Bill Depth <= 20 mm") {
      filtered_penguins <- subset(penguins, bill_depth_mm >= 20 & bill_depth_mm < 30)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Bill Depth (<=20 mm) (Deep) by Species")  
    } else if (input$chartType == "Bill Length 1-34 mm") {
      filtered_penguins <- subset(penguins, bill_length_mm >= 1 & bill_length_mm < 35)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Bill Length (1-34 mm) (Short) by Species")  
    } else if (input$chartType == "Bill Length 34-44 mm") {
      filtered_penguins <- subset(penguins, bill_length_mm >= 34 & bill_length_mm < 45)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Bill Length (34-44 mm) (Medium) by Species")  
    } else if (input$chartType == "Bill Length 45-60 mm") {
      filtered_penguins <- subset(penguins, bill_length_mm >= 45 & bill_length_mm < 60)
      
      species_counts <- table(filtered_penguins$species)
      
      pie_chart <- plot_ly(labels = names(species_counts), values = species_counts, type = 'pie',
                           textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = minimal_colors),
                           hole = 0.4) %>%
        layout(title = "Bill Length (45-60 mm) (Long) by Species")  
      
    }
    
    pie_chart
  })
  
  output$pairsPlot <- renderPlot({
    if (input$chartType == "Species Distribution") {
      species_counts <- table(penguins$species)
      ggplot(data = penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Sex Ratio") {
      sex_counts <- table(penguins$sex)
      ggplot(data = penguins, aes(x = sex, fill = sex)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Flipper Length Ranges") {
      penguins$flipper_length_category <- sapply(penguins$flipper_length_mm, categorize_flipper_length)
      ggplot(data = penguins, aes(x = f
                                  lipper_length_category, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Bill Depth Categories") {
      penguins$bill_depth_category <- sapply(penguins$bill_depth_mm, categorize_bill_depth)
      ggplot(data = penguins, aes(x = bill_depth_category, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Bill Length Categories") {
      penguins$bill_length_category <- sapply(penguins$bill_length_mm, categorize_bill_length)
      ggplot(data = penguins, aes(x = bill_length_category, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Flipper Length 160-179 mm") { 
      filtered_penguins <- subset(penguins, flipper_length_mm >= 160 & flipper_length_mm < 180)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Flipper Length 180-199 mm") { 
      filtered_penguins <- subset(penguins, flipper_length_mm >= 180 & flipper_length_mm < 200)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Flipper Length 200-219 mm") { 
      filtered_penguins <- subset(penguins, flipper_length_mm >= 200 & flipper_length_mm < 220)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Flipper Length 220-239 mm") { 
      filtered_penguins <- subset(penguins, flipper_length_mm >= 220 & flipper_length_mm < 240)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Bill Depth 1-14 mm") { 
      filtered_penguins <- subset(penguins, bill_depth_mm >= 1 & bill_depth_mm < 15)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Bill Depth 15-19 mm") { 
      filtered_penguins <- subset(penguins, bill_depth_mm >= 15 & bill_depth_mm < 20)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Bill Depth <= 20 mm") { 
      filtered_penguins <- subset(penguins, bill_depth_mm >= 20 & bill_depth_mm < 30)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
      # may wrong
    } else if (input$chartType == "Bill Length 1-34 mm") { 
      filtered_penguins <- subset(penguins, bill_length_mm >= 1 & bill_length_mm < 35)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack") 
    } else if (input$chartType == "Bill Length 34-44 mm") { 
      filtered_penguins <- subset(penguins, bill_length_mm >= 34 & bill_length_mm < 45)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")
    } else if (input$chartType == "Bill Length 45-60 mm") { 
      filtered_penguins <- subset(penguins, bill_length_mm >= 45 & bill_length_mm < 60)
      ggplot(data = filtered_penguins, aes(x = species, fill = species)) +
        geom_bar() +
        labs(title = "BadStack")  
    }
  })
  
  output$individualplot <- renderPlot({
    if(input$penid == 0) {
    } else {
      select_penguin <- as.data.frame(na.omit(penguins %>% select(bill_length_mm,
                                                                  bill_depth_mm,
                                                                  flipper_length_mm,
                                                                  body_mass_g,
                                                                  sex,
                                                                  year,
                                                                  island,
                                                                  species)))
      
      penguins_data <- data.frame(
        row.names = "Penguin1",
        Bill_length = select_penguin$bill_length_mm[input$penid],
        Bill_depth = select_penguin$bill_depth_mm[input$penid],
        Flipper_length = select_penguin$flipper_length_mm[input$penid],
        Body_mass = select_penguin$body_mass_g[input$penid]
      )
      
      max_min <- data.frame(
        Bill_length = c(50, 0), Bill_depth = c(30, 0), Flipper_length = c(250, 0),
        Body_mass = c(5000, 0)
      )
      
      rownames(max_min) <- c("Max", "Min")
      
      # Bind the variable ranges to the data
      df <- rbind(max_min, penguins_data)
      
      penguin_chart <- df[c("Max", "Min", "Penguin1"), ]
      radarchart(penguin_chart)
    }
  })
  
  output$individualsex <- renderText({
    if(input$penid == 0) {
    } else {
      paste0("Sex: ", select_penguin$sex[input$penid])
    }})
  output$individualyear <- renderText({
    if(input$penid == 0) {
    } else {paste0("Year: ", select_penguin$year[input$penid])
    }})
  output$individualisland <- renderText({
    if(input$penid == 0) {
    } else {paste0("Island: ", select_penguin$island[input$penid])
    }})
  output$individualspecies <- renderText({
    if(input$penid == 0) {
    } else {paste0("Species: ", select_penguin$species[input$penid])
    }})

}
shinyApp(ui = ui, server = server)
