# Function for module UI
input_file_upload_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(12, numericInput(ns("analyt_start_row"), "Analyte Start Row:", 26, min = 1, max = 100)),
    column(12, fileInput(ns("raw_file"), label = "Choose files",
                         multiple = TRUE,
                         buttonLabel = "Browse or Drop...",
                         placeholder = "(multiple) xls")),
    column(12, actionButton(ns("add_upload"), label = "UPLOAD"))
  )
}


# Function for module server logic
input_file_upload <- function(input, output, session, values) {
  
  observeEvent(input$add_upload, {
    req(input$raw_file)
    #message(input$raw_file$name)
    validate(need(is.integer(input$analyt_start_row) & input$analyt_start_row > 0,
                  "analyt_start_row must be a intiger greater than 0, check xls's analyte tab!"))
    
    temp_ls <- list()
    
    for(x in input$raw_file$name){
      temp_ls[[x]] <- shinyCatch(
        suppressWarnings(
          suppressMessages(
            read_lmx(input$raw_file$datapath[input$raw_file$name == x],  analyt_start_row = input$analyt_start_row)
          )
        ),
        blocking_level = "error", trace_back = F)
      
      temp_ls[[x]][["File"]] <- x
      #colnames(temp_ls[[x]]) <- temp_ls[[x]]$Assay
    }
    
    values$upload_data <- c(values$upload_data, temp_ls)

  })
  
}

