# Function for module UI
reset_dataset_UI <- function(id) {
  
  ns <- NS(id)
  
  column(12, actionButton(ns("reset_dataset"), "Clear Dataset, Restart Upload"))
  
}


# Function for module server logic
reset_dataset <- function(input, output, session, values) {
  
  observeEvent(input$reset_dataset,{
    
    values$upload_data = NULL  
    values$ref_sample_identifier = NULL
    values$norm_method = NULL
    values$normed_data = NULL
    values$combined_data = NULL
    values$combined_meta = NULL
    values$pca_fit = NULL
    
    return(values)

    shinyCatch(message("Dataset has been cleared..."))
  })
  
}