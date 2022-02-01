# Function for module UI
corr_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(3, selectInput(ns("choice_b0"), "Choose Dataset A", choices = "", selected = NULL)),
    column(3, selectInput(ns("choice_b1"), "Choose Dataset B", choices = "", selected = NULL)),
    column(3, selectInput(ns("choice_b2"), "Choose Analyte", choices = "", selected = NULL)),
    column(12, plotOutput(ns("corr_plot"),width = "100%", height = "500px")))
  
}


# Function for module server logic
corr_plot <- function(input, output, session, values) {
  
  # corr plot
  observe({
    updateSelectInput(session, "choice_b0", choices = names(values$upload_data))
  })
  observe({
    updateSelectInput(session, "choice_b1", choices = names(values$upload_data))
  })
  observe({
    updateSelectInput(session, "choice_b2", 
                      choices = intersect(rownames(values$upload_data[[input$choice_b0]]), rownames(values$upload_data[[input$choice_b1]])))
  })
  
  cor_df <- reactive({
    req(values$upload_data)
    #common_sample <- intersect(values$upload_data[[input$choice_b0]]$Assay, values$upload_data[[input$choice_b1]]$Assay)
    common_sample <- intersect(values$combined_meta$Sample[values$combined_meta$File == input$choice_b0], 
                               values$combined_meta$Sample[values$combined_meta$File == input$choice_b1])
    
    data.frame(A = values$upload_data[[input$choice_b0]]@assays@data$data_imputed[
      rownames(values$upload_data[[input$choice_b0]]) == input$choice_b2, 
      #match(common_sample, values$upload_data[[input$choice_b0]]$Assay)
      match(common_sample, values$combined_meta$Sample[values$combined_meta$File == input$choice_b0])
    ]%>%as.numeric(),
    B = values$upload_data[[input$choice_b1]]@assays@data$data_imputed[
      rownames(values$upload_data[[input$choice_b1]]) == input$choice_b2, 
      #match(common_sample, values$upload_data[[input$choice_b1]]$Assay)]%>%as.numeric(),
      match(common_sample, values$combined_meta$Sample[values$combined_meta$File == input$choice_b1])
    ]%>%as.numeric(),
    Sample = common_sample)%>%
      filter((!is.na(A) & !is.na(B)))
  })
  
  output$corr_plot <- renderPlot({
    cor_df()%>%
      ggscatter(x = "A", y = "B", palette = "jco",
                add = "reg.line", conf.int = F)+
      stat_cor(label.x.npc = 0.2, size = 6)+
      labs(title = input$choice_b2)
  })
}