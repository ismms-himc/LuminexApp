library(shiny)
library(shinydashboard)

server <- function(input, output, session) {
  
  values <- reactiveValues(upload_data = NULL,  
                           ref_sample_identifier = NULL,
                           norm_method = NULL,
                           normed_data = NULL,  # not used for now
                           combined_data = NULL,
                           combined_meta = NULL,
                           pca_fit = NULL)
  
  callModule(module = reset_dataset, id = "id_1", values)
  
  callModule(module = input_file_upload, id = "id_1", values)
  
  callModule(module = show_file_level_meta, id = "id_1", values)
  
  callModule(module = meta_view_replace, id = "id_1", values)
  
  callModule(module = identify_reference, id = "id_1", values)
  
  callModule(module = ref_normalization, id = "id_1", values)
  
  callModule(module = creat_data_obj, id = "id_1", values)
  
  callModule(module = download_qa_report, id = "id_1", values)
  
  callModule(module = download_combined_data, id = "id_1", values)
  
  callModule(module = scatter_plot, id = "id_1", values)
  
  callModule(module = download_scatter_report, id = "id_1", values)
  
  callModule(module = pca_plot, id = "id_1", values)
  
  callModule(module = corr_plot, id = "id_1", values)
  
  callModule(module = download_cor_report, id = "id_1", values)
  
}