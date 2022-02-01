# Function for module UI
download_combined_data_UI <- function(id) {
  
  ns <- NS(id)
  
  downloadButton(ns("export_cmb"), "Download Combined Data")
  
}


# Function for module server logic
download_combined_data <- function(input, output, session, values) {
  
  output$export_cmb <- downloadHandler(
    filename = function() {
      paste0("Exported_", gsub("-", "_", Sys.Date()), ".zip")
    },
    content = function(file){
      
      tmpdir <- tempdir()
      on.exit(setwd(tmpdir))
      print(tmpdir)
      
      # for clustergrammer
      
      clustergm <- lapply(names(values$combined_meta), function(x){
        paste0(gsub("(\\.|-)", "_", x), ": ",values$combined_meta[[x]])
      })%>%
        do.call(what = "rbind")%>%
        rbind(t(scale(t(values$combined_data@assays@data$data_imputed))))%>%
        set_colnames(value = paste0("id: c", 1 : ncol(values$combined_data)))
      
      rownames(clustergm) <- ifelse(rownames(clustergm) %in% rownames(values$combined_data@assays@data$data_imputed), 
                                    paste0("analyte: ", rownames(clustergm)), "")
      
      if(!is.null(values$normed_data)){
        files <- c("Meta.csv", "Feature.csv", "Raw_conc.csv", "Imputed_conc.csv", 
                   "Raw_MFI.csv", "clustergm.csv", "Normed_conc.csv", "Normed_MFI.csv")
        
        write.csv(data.frame(values$combined_meta), 
                  file = "Meta.csv", row.names = F)
        write.csv(data.frame(values$combined_data@elementMetadata@listData), 
                  file = "Feature.csv", row.names = F)
        write.csv(values$combined_data@assays@data$data_default, 
                  file = "Raw_conc.csv", row.names = T)
        write.csv(values$combined_data@assays@data$data_imputed, 
                  file = "Imputed_conc.csv", row.names = T)
        write.csv(values$combined_data@assays@data$mfi_default, 
                  file = "Raw_MFI.csv", row.names = T)
        write.csv(clustergm, 
                  file = "clustergm.csv", row.names = T)
        write.csv(values$combined_data@assays@data$data_default_normed, 
                  file = "Normed_conc.csv", row.names = T)
        write.csv(values$combined_data@assays@data$mfi_default_normed, 
                  file = "Normed_MFI.csv", row.names = T)
        zip(file,files)
      }
      else{
        files <- c("Meta.csv", "Feature.csv", "Raw_conc.csv", "Imputed_conc.csv", 
                   "Raw_MFI.csv", "clustergm.csv")
        
        write.csv(data.frame(values$combined_meta), 
                  file = "Meta.csv", row.names = F)
        write.csv(data.frame(values$combined_data@elementMetadata@listData), 
                  file = "Feature.csv", row.names = F)
        write.csv(values$combined_data@assays@data$data_default, 
                  file = "Raw_conc.csv", row.names = T)
        write.csv(values$combined_data@assays@data$data_imputed, 
                  file = "Imputed_conc.csv", row.names = T)
        write.csv(values$combined_data@assays@data$mfi_default, 
                  file = "Raw_MFI.csv", row.names = T)
        write.csv(clustergm, 
                  file = "clustergm.csv", row.names = T)
        zip(file,files)
      }
    }
  )
}