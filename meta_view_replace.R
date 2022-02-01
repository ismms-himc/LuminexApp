# Function for module UI
meta_view_replace_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(6, downloadButton(ns("export_meta"), "Download Current Meta Data")),
    column(6, actionButton(ns("add_meta"), "Replace Meta Data file")),
    column(12, fileInput(ns('import_meta'), label = "Upload Modified Meta Data",
                        multiple = F,
                        buttonLabel = "Browse or Drop...")),
    column(12, DT::DTOutput(ns("meta_table")))
  )
  
}


# Function for module server logic
meta_view_replace <- function(input, output, session, values) {
  
  #download metadata for user modification 
  output$export_meta <- downloadHandler(
    filename = function() {
      paste0("Exported_meta", gsub("-", "_", Sys.Date()), ".csv")
    },
    content = function(file){
      
      tmpdir <- tempdir()
      on.exit(setwd(tmpdir))
      print(tmpdir)
      
      write.csv(data.frame(
        #values$combined_data@colData@listData
        values$combined_meta
      ), 
      file, row.names = F)
      
    }
  )  
  
  # render and update update metadata
  proxy <- DT::dataTableProxy("meta_table")
  
  observeEvent(input$add_meta, {
    req(input$import_meta)
    message(input$import_meta$name)
    
    values$combined_meta <- read.csv(input$import_meta$datapath)
    DT::replaceData(proxy, values$combined_meta, resetPaging = FALSE)
    
    shinyCatch(message("Meta data is updated!"))
  })
  
  
  output$meta_table <- DT::renderDT({
    DT::datatable(values$combined_meta, editable = TRUE, options = list(scrollX = TRUE))
    
  })
}