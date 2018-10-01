library(tidyverse)

ui <- fluidPage(
  
  # Application title
  titlePanel("Summarize GSEA Output"),
  
  # Sidebar with input for data and user options 
  sidebarLayout(
    sidebarPanel(
      print(strong("1) Input your file:")),
      
      fileInput(inputId = "gsea.data", label = "GSEA Report (must be .csv format)"),

      print(strong("2) Specify directionality & cutoff:")),
      
      checkboxInput(inputId = "below.zero", label = "NES < 0?", value = FALSE),
      
      numericInput(inputId = "NES.cut", label = "NES Cutoff", value = 1.5),
      br(),
      
      print(strong("3) Manipulate downloaded plot:")),

      numericInput(inputId = "text.size", label = "Text Size", value = 14),
      
      numericInput(inputId = "width", label = "Width of Downloaded Plot (in.)", value = 8),
      
      numericInput(inputId = "height", label = "Height of Downloaded Plot (in.)", value = 4),
      
      downloadButton("download.plot", "Download Plot")
    ),
    
    # Main panel output
    mainPanel(
      
      # Preview of user data
      print(strong("Preview of your data:")),
      br(),
      tableOutput("prev.table"),
      br(),
      
      # NES/FDR plot based on user-defined cutoff
      print(strong("NES & FDR of deregulated gene sets surpassing cutoff:")),
      br(), br(),
      plotOutput("plot.data"),
      br(), br(), br(), br()
      
    )
  )
)

# Define server logic required to return data and graphs
server <- function(input, output, session) {
  # Preview of data
  dat.table <- eventReactive(input$gsea.data, {
    read.csv(input$gsea.data$datapath)
  })
  
  output$prev.table <- renderTable({
    dat.table() %>% 
      setNames(c("name", "msigdb", "details", "size", "ES", "NES", "pval", "pfdr", "pfwer", "rank", "leading_edge")) %>%
      select(-(msigdb:details)) %>%
      head(4)
  })
  
  # NES/FDR plot based on user-defined cutoff

  output$plot.data <- renderPlot({
    
    if(input$below.zero == FALSE) {
    
    dat.table() %>%
      setNames(c("name", "msigdb", "details", "size", "ES", "NES", "pval", "pfdr", "pfwer", "rank", "leading_edge")) %>%
      filter(NES > input$NES.cut) %>% 
      ggplot(aes(reorder(name, NES), NES, fill = pfdr)) +
      geom_col() +
      scale_fill_gradient(low = "blue4", high = "slategray1") +
      labs(x = "", fill = "FDR") +
      coord_flip() +
      theme_bw(base_size = input$text.size) +
      theme(axis.text = element_text(colour = "black"),
            legend.key.size = unit(0.5, "cm"),
            legend.margin = margin(0,0,0,-2))
    } else {
      dat.table() %>%
        setNames(c("name", "msigdb", "details", "size", "ES", "NES", "pval", "pfdr", "pfwer", "rank", "leading_edge")) %>%
        filter(NES < input$NES.cut) %>% 
        ggplot(aes(reorder(name, NES), NES, fill = pfdr)) +
        geom_col() +
        scale_fill_gradient(low = "blue4", high = "slategray1") +
        labs(x = "", fill = "FDR") +
        coord_flip() +
        theme_bw(base_size = input$text.size) +
        theme(axis.text = element_text(colour = "black"),
              legend.key.size = unit(0.5, "cm"),
              legend.margin = margin(0,0,0,-2))
    }
    })
  
  
  # For the download button
  plot.to.save <- reactive({
   
     if(input$below.zero == FALSE) {
      
      dat.table() %>%
        setNames(c("name", "msigdb", "details", "size", "ES", "NES", "pval", "pfdr", "pfwer", "rank", "leading_edge")) %>%
        filter(NES > input$NES.cut) %>% 
        ggplot(aes(reorder(name, NES), NES, fill = pfdr)) +
        geom_col() +
        scale_fill_gradient(low = "blue4", high = "slategray1") +
        labs(x = "", fill = "FDR") +
        coord_flip() +
        theme_bw(base_size = input$text.size) +
        theme(axis.text = element_text(colour = "black"),
              legend.key.size = unit(0.5, "cm"),
              legend.margin = margin(0,0,0,-2))
    } else {
      dat.table() %>%
        setNames(c("name", "msigdb", "details", "size", "ES", "NES", "pval", "pfdr", "pfwer", "rank", "leading_edge")) %>%
        filter(NES < input$NES.cut) %>% 
        ggplot(aes(reorder(name, NES), NES, fill = pfdr)) +
        geom_col() +
        scale_fill_gradient(low = "blue4", high = "slategray1") +
        labs(x = "", fill = "FDR") +
        coord_flip() +
        theme_bw(base_size = input$text.size) +
        theme(axis.text = element_text(colour = "black"),
              legend.key.size = unit(0.5, "cm"),
              legend.margin = margin(0,0,0,-2))
    }
  })
  
  output$download.plot <- downloadHandler(
    filename = "GSEA_summary.png",
    content = function(file) {
      ggsave(file, plot = plot.to.save(), device = "png", dpi = 300, width = input$width, height = input$height)
    }
  )
  
}
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)