
# Function for module UI
show_file_level_meta_UI <- function(id) {
  
  ns <- NS(id)
  
  DT::DTOutput(ns("plate_summary"))
  
}


# Function for module server logic
show_file_level_meta <- function(input, output, session, values) {

  output$plate_summary <- DT::renderDT(
    DT::datatable(
      data.frame(filename = sapply(values$upload_data, function(x) x$File[1]) %>% unlist(),
                 #panel = sapply(values$upload_data, function(x) x@metadata$panel)%>% unlist(),
                 total_samples = sapply(values$upload_data, function(x) ncol(x)) %>% unlist(),
                 total_analyte = sapply(values$upload_data, function(x) nrow(x)) %>% unlist()),
      rownames = NULL)
  )
}