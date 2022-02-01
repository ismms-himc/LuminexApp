
# Function for module UI
pca_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(12, selectInput(ns("choice_c0"), "Choose Dataset Type", choices = "", selected = NULL)),
    column(12, actionButton(ns("run_pca"), "Run PCA")),
    column(6, selectInput(ns("choice_c1"), "Choose Color Variable", choices = "", selected = "File")),
    column(6, selectInput(ns("choice_c2"), "Choose label var", choices = "", selected = "File")),
    column(12, plotOutput(ns("pca_plot"),width = "100%", height = "500px"))
  )
  
}


# Function for module server logic
pca_plot <- function(input, output, session, values) {
  
  # pca plot 
  observe({
    req(values$combined_data)
    updateSelectInput(session, "choice_c0", choices = names(values$combined_data@assays@data))
  })
  observe({
    updateSelectInput(session, "choice_c1", choices = names(values$combined_meta))
  })
  observe({
    updateSelectInput(session, "choice_c2", choices = names(values$combined_meta))
  })
  
  
  observeEvent(input$run_pca, {
    req(values$combined_data)
    # simple mean impute for na
    mat <- values$combined_data@assays@data[[input$choice_c0]]
    mat <- apply(mat, 2, function(x){
      temp <- x
      temp[!is.finite(temp)] <- NA
      temp[is.na(temp)] <- median(temp, na.rm = T)
      shinyCatch(message("Out of range values were imputated with analyte wise median"))
      temp
    })
    set.seed(1234)
    values$pca_fit <- prcomp(t(mat))
    values$pca_fit$plot_df <- data.frame(values$combined_meta,
                                         pc_1 = values$pca_fit$x[ ,1],
                                         pc_2 = values$pca_fit$x[ ,2])
  })
  
  
  output$pca_plot <- renderPlot({
    req(values$pca_fit)
    
    values$pca_fit$plot_df%>%
      ggplot(aes_string("pc_1", "pc_2", color = input$choice_c1), alpha = 0.7, shape = 21)+
      geom_point()+
      labs(x     = paste0("1st dimension (",
                          round((values$pca_fit$sdev^2/sum(values$pca_fit$sdev^2))[1] * 100),
                          "%)"),
           y     = paste0("2nd dimension (",
                          round((values$pca_fit$sdev^2/sum(values$pca_fit$sdev^2))[2] * 100),
                          "%)"))+
      stat_ellipse()+
      ggrepel::geom_text_repel(aes_string(label = input$choice_c2))+
      theme_bw()+
      guides(color = "none")
    
  })
}