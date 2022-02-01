# Function for module UI
creat_data_obj_UI <- function(id) {
  ns <- NS(id)
  actionButton(ns("creat_data_obj"), "Creat Data Object")
}


# Function for module server logic
creat_data_obj <- function(input, output, session, values) {
  
  #creat data object combine files
  observeEvent(input$creat_data_obj, {
    if(length(values$upload_data) == 1){
      values$combined_data <- values$upload_data[[1]]
    }
    else if(!is.null(values$normed_data)){
      values$combined_data <- cmb_lmx_se(values$normed_data) #?? clean
    }else{
      values$combined_data <- cmb_lmx_se(values$upload_data)
    }
    values$combined_meta <- data.frame(values$combined_data@colData)
    ref_pattern <- paste(strsplit(input$ref_idf_str, split = ",")%>%unlist(), collapse = "|")
    ref_pattern <- paste0("(", ref_pattern, ")")
    values$combined_meta$ref_sample <- ifelse(grepl(ref_pattern, ignore.case = T, values$combined_meta$Sample), "ref_sample", "study_sample")
    values$ref_sample_identifier <- input$ref_idf_str # update ref_sample sotred, might be different from Normalization 
    shinyCatch(message("Data Object is Created"))
  })
}