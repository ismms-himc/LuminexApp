

# Function for module UI
download_qa_report_UI <- function(id) {
  
  ns <- NS(id)
  
  downloadButton(ns("download_qa_report"), "Download Data Report")
}


# Function for module server logic
download_qa_report <- function(input, output, session, values) {
  
  output$download_qa_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      #tmpdir <- tempdir()
      #on.exit(setwd(tmpdir))
      #print(tmpdir)
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        rp_upload_data = values$upload_data,
        rp_ref_idf_str = values$ref_sample_identifier,
        rp_combined_data = values$combined_data,
        rp_combined_meta = values$combined_meta
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}