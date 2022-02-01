
library(shiny)
library(shinydashboard)

# header
header <- dashboardHeader(title = "LuminexApp",
                          tags$li(class = "dropdown", actionButton("browser", "browser"), tags$script("$('#browser').hide();")),
                          dropdownMenuOutput("messageMenu"))
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem( "Upload Files", tabName = 'upload_files', icon = icon('import', lib = 'glyphicon')),
    menuItem( "Visulization", tabName = 'data_vis', icon = icon('th-list', lib = 'glyphicon'))
  )
)

# p1.file upload box
input_file_upload_box <- box(
  title = "Upload Data",
  status = "info", 
  solidHeader = TRUE, 
  width = 12,
  fluidRow(
    column(12, h4(icon("circle"), "Accept one or multiple Luminex xls files of the Same Panel."))),
  fluidRow(
    column(12, h4(icon("circle"), "Open the raw Luminex xls file, view a individual 'Analyte Tab' to identified the 'Analyte Start Row'.
                    'Analyte Start Row' is common across analytes in the same xls file."))),
  input_file_upload_UI(id = "id_1"),
  fluidRow(reset_dataset_UI(id = "id_1"))
)

# p1.plate summary box
file_meta_summary_box <- box(
  title = "Uploaded plate summary stat",
  collapsible = TRUE,
  # collapsed = TRUE,
  status = "primary", solidHeader = TRUE, width = 12,
  fluidRow(
    column(12, h4(icon("database"), "Uploaded plate summary stat")),
    column(12, show_file_level_meta_UI(id = "id_1"))
  )
)


# p1.reference & normalization box
reference_normalization_box <- box(
  title = "Identify QC/Reference Sample (optional Ref-sample-based-normalization)",
  collapsible = TRUE,
  #collapsed = TRUE,
  status = "warning", solidHeader = TRUE, width = 12,
  identify_reference_UI(id = "id_1"),
  fluidRow(
    column(12, h4(icon("star"), "Optional Ref-sample-based-normalization")),
    column(12, h4(icon("circle"), "Method: Reference-sample-based normalization The plate-to-plate variation was adjusted based on 
                                   adjust-factor(plate specific) which makes equal NPX value among reference samples (common reference run on each plate)."))),
  ref_normalization_UI(id = "id_1")
)


# p1.object fuctional box----
obj_fun_box <- box(
  title = "Creat Data Object",
  collapsible = TRUE,
  # collapsed = TRUE,
  status = "info", solidHeader = TRUE, width = 12,
  fluidRow(
    column(12, h4(icon("circle"), "Creat Data Object for Visulization and Download.")),
    column(12, h4(icon("circle"), "4 dataset will be generated: ")),
    column(12, offset = 1, h4("data_default: the Concentration data, out of range value read as NA")),
    column(12, offset = 1, h4("data_imputed: the Concentration data, out of range value read as Lower or Higher detection limit")),
    column(12, offset = 1, h4("mfi_default: the MFI data")),
    column(12, h4(icon("circle"), "If normalization is done, addational data_default_normed and mfi_normed datasets will be generated."))),
  fluidRow(
    column(4, creat_data_obj_UI(id = "id_1"))),
  fluidRow(
    column(4, download_qa_report_UI(id = "id_1")),
    column(4, download_combined_data_UI(id = "id_1")))
)


# p1.meta data view or replace box
meta_view_replace_box <- box(title = "Metadata Setup",
                             collapsible = TRUE,
                             #collapsed = TRUE,
                             status = "primary", solidHeader = TRUE, width = 12,
                             fluidRow(
                               column(12, h4(icon("circle"), "Download metadata for optional user input column, Tiempoint or Sample_Type for example.")),
                               column(12, h4(icon("circle"), "Upload modified metadata to replace the current data in display")),
                               column(12, h4(icon("star"), "***DO NOT change Row Orders!!!, after update, run 'Creat Data Object' to apply the change for visulization.")),
                             ),
                             meta_view_replace_UI(id = "id_1")
)


# p2.scatter plot box
scatter_plot_box <- box(title = "Scatter Plot",
                        collapsible = T,
                        collapsed = T,
                        status = "warning", solidHeader = T, width = 12,
                        scatter_plot_UI(id = "id_1")
                        )

# p2. pca plot box
pca_plot_box <- box(title = "PCA Plot",
                    collapsible = T,
                    collapsed = T,
                    status = "info", solidHeader = T, width = 12,
                    fluidRow(
                      column(12, h4(icon("circle"), "Select dataset and click 'Run PCA' to generate the PCA plot.")),
                      column(12, offset = 1, h4("data_default/normed: the out of range NA value were imputated with simple analyte-wise 'median'. Interprated with caution.")),
                      column(12, offset = 1, h4("data_imputed: the out of range value read as Lower or Higher detection limit. Interprated with caution.")),
                      column(12, offset = 1, h4("mfi_default/normed: MFI data, none-manapulated data from assay result."))),
                    pca_plot_UI(id = "id_1")
                    )

# p3. corr plot box
corr_plot_box <- box(title = "Correlation Plot--Based on NoneNormalized Data",
                     collapsible = T,
                     collapsed = T,
                     status = "primary", solidHeader = T, width = 12,
                     corr_plot_UI(id = "id_1")
                     )

# body ----
body <- dashboardBody(
  
  add_busy_bar(color = "blue", height = "8px"),
  
  tabItems(tabItem(tabName = "upload_files",
                   input_file_upload_box,
                   file_meta_summary_box,
                   reference_normalization_box,
                   obj_fun_box,
                   meta_view_replace_box
                   ),
           
           tabItem(tabName = "data_vis",
                   fluidRow(scatter_plot_box),
                   fluidRow(pca_plot_box),
                  fluidRow(corr_plot_box)
           )
  )
  
)

# assemble UI
ui <- dashboardPage(header, sidebar, body, skin = "green")