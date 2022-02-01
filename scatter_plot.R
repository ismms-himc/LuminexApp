
# Function for module UI
scatter_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(3, selectInput(ns("choice_a0"), "Choose Analyte", choices = "", selected = NULL)),
    column(3, selectInput(ns("choice_a1"), "Choose x asix var", choices = "", selected = "File")),
    column(3, selectInput(ns("choice_a2"), "Choose color var", choices = "", selected = "File")),
    column(3, selectInput(ns("choice_a3"), "Choose label var", choices = "", selected = NULL)),
    column(3, selectInput(ns("choice_a4"), "Choose data set", choices = "", selected = "data_default")),
    column(12, plotlyOutput(ns("scatter_plot"),width = "100%", height = "800px"))
  )
  
}


# Function for module server logic
scatter_plot <- function(input, output, session, values) {
  
  # scatter plot
  observe({
    updateSelectInput(session, "choice_a0", choices = rownames(values$combined_data)) # analyte
  })
  observe({
    updateSelectInput(session, "choice_a1", choices = names(values$combined_meta), selected = "File") # x var
  })
  observe({
    updateSelectInput(session, "choice_a2", choices = names(values$combined_meta), selected = "File") # color var
  })
  
  observe({
    updateSelectInput(session, "choice_a3", choices = names(values$combined_meta)) # label var
  })
  
  observe({
    req(values$combined_data)
    updateSelectInput(session, "choice_a4", choices = names(values$combined_data@assays@data)) # dataset
  })
  
  
  
  output$scatter_plot <- renderPlotly({
    req(values$combined_data)
    
    
    if(length(unique(values$combined_meta$File)) > 1){
      values$combined_data@elementMetadata$LOD <- NULL
      values$combined_data@elementMetadata$HOD <- NULL
    }
    lod <- data.frame(analyt = rownames(values$combined_data),
                      lapply(names(values$combined_data@elementMetadata)[grep("(LOD_|LOD)", names(values$combined_data@elementMetadata))], function(x){
                        values$combined_data@elementMetadata[[x]]
                      })%>%
                        do.call(what = "cbind")%>%
                        set_colnames(value = unique(values$combined_data$File))
    )%>%
      gather(-analyt, key = "File", value = "lod")%>%
      mutate(File = gsub("LOD_", "", File),
             lod = log10(lod))
    
    hod <- data.frame(analyt = rownames(values$combined_data),
                      lapply(names(values$combined_data@elementMetadata)[grep("(HOD_|HOD)", names(values$combined_data@elementMetadata))], function(x){
                        values$combined_data@elementMetadata[[x]]
                      })%>%
                        do.call(what = "cbind")%>%
                        set_colnames(value = unique(values$combined_data$File))
    )%>%
      gather(-analyt, key = "File", value = "hod")%>%
      mutate(File = gsub("HOD_", "", File),
             hod = log10(hod))
    
    
    p <- plot_ly(cbind.data.frame(
      values$combined_meta,
      log10_scale = values$combined_data@assays@data[[input$choice_a4]][rownames(values$combined_data) == input$choice_a0, ]%>%as.numeric()%>%log10()
    )%>%
      left_join(
      data.frame(File = unique(values$combined_meta$File))%>%
        merge.data.frame(lod)%>%
        filter(analyt == input$choice_a0)%>%
        dplyr::select(-analyt),
      by = "File")%>% 
      left_join(
        data.frame(File = unique(values$combined_meta$File))%>%
          merge.data.frame(hod)%>%
          filter(analyt == input$choice_a0)%>%
          dplyr::select(-analyt),
        by = "File"), 
    x = ~get(input$choice_a1), 
    y = ~log10_scale, 
    type = 'box', 
    color = ~get(input$choice_a2), 
    text = ~get(input$choice_a3), 
    boxpoints = "all", jitter = 0.7, pointpos = 0, hoverinfo = "text")
    
    if(!grepl("(mfi|normed)", ignore.case = T, input$choice_a4)){
      p%>%
        add_text(x = ~get(input$choice_a1), y = ~lod, text = "LOD", textposition = "left",marker = list(color = 'rgb(0, 0, 0)', symbol = "arrow-up"))%>% 
        add_text(x = ~get(input$choice_a1), y = ~hod, text = "LOD", textposition = "left",marker = list(color = 'rgb(0, 0, 0)', symbol = "arrow-down"))%>% 
        layout(boxmode = "group", xaxis = list(title = ""))
    }else{
      p%>%
        layout(boxmode = "group", xaxis = list(title = ""))
    }
  })
}


