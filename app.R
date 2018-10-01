library(tidyverse)

ui <- fluidPage(
  
  # Application title
  titlePanel("Summarize GSEA Output"),
  
  # Sidebar with input for data and user options 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "gsea.data", label = "GSEA Report (.csv format)"),
      br(), br(),
      
      numericInput(inputId = "NES.max", label = "Max. NES Cutoff (for Upregulated pathways)", value = 1.5),
      br(),
      numericInput(inputId = "NES.min", label = "Min. NES Cutoff (for Downregulated pathways)", value = -1.5)
    ),
    
    # Main panel output
    mainPanel(
      
      # Preview of user data
      print("Preview of your data:"),
      br(),
      tableOutput("prev.table"),
      br(),
      
      # NES/FDR plot based on user-defined cutoff for UPREGULATED
      print("NES & FDR of top upregulated gene sets"),
      br(), br(),
      plotOutput("plot.up"),
      br(), br(), br(), br(),
      
      
      # NES/FDR plot based on user-defined cutoff for DOWNREGULATED
      print("NES & FDR of top downregulated gene sets"),
      br(), br(),
      plotOutput("plot.down")
    )
  )
)

# Define server logic required to return data and graphs
server <- function(input, output, session) {
  # Preview of data
  dat.table <- eventReactive(input$gsea.data, {
    read.csv(input$gsea.data$datapath)
  })
  
  #output$prev.table <- renderTable({
  #  dat.table() %>% setNames(c("name", "msigdb", "details", "size", "ES", "NES", "pval", "pfdr", "pfwer", "rank", "leading_edge")) %>% head(4)
  #})
  
  # NES/FDR plot based on user-defined cutoff for UPREGULATED
#  max.cut <- input$NES.max
  
 # output$plot.up <- renderPlot({
#    expr = dat.table() %>%
#      setNames(c("name", "msigdb", "details", "size", "ES", "NES", "pval", "pfdr", "pfwer", "rank", "leading_edge")) %>%
#      filter(NES > max.cut) %>% 
#      ggplot(aes(reorder(name, NES), NES, fill = pfdr)) +
#      geom_col() +
#      scale_fill_gradient(low = "blue4", high = "slategray1") +
#      labs(x = "", fill = "FDR") +
#      coord_flip() +
#      theme_bw(base_size = 8) +
#      theme(axis.text = element_text(colour = "black"),
#            legend.key.size = unit(0.3, "cm"),
#            legend.margin = margin(0,0,0,-2))
#    })
  
  # NES/FDR plot based on user-defined cutoff for DOWNREGULATED
#  min.cut <- input$NES.min
  
#  output$plot.up <- renderPlot({
 #   expr = dat.table() %>%
  #    setNames(c("name", "msigdb", "details", "size", "ES", "NES", "pval", "pfdr", "pfwer", "rank", "leading_edge")) %>%
  #    filter(NES < min.cut) %>% 
  #    ggplot(aes(reorder(name, -NES), NES, fill = pfdr)) +
  #    geom_col() +
  #    scale_fill_gradient(low = "blue4", high = "slategray1") +
  #    labs(x = "", fill = "FDR") +
  #    coord_flip() +
  #    theme_bw(base_size = 8) +
  #    theme(axis.text = element_text(colour = "black"),
  #          legend.key.size = unit(0.3, "cm"),
  #          legend.margin = margin(0,0,0,-2))
  #  })
}
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)