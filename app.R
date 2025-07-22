#loading libraries
library(shiny)
library(bslib)
library(shinyWidgets)
library(yaml)

#load UI components for the intro page
source("scripts/intro_UI.R")

#load UI components for the specification page
source("scripts/specification_UI.R")

#load UI components for the about page
source("scripts/about_UI.R")

#load app_title, app_footer
source("scripts/general_UI.R")

#load function for download handling
source("scripts/download_handler.R", local = TRUE)

# --- Shiny app -------------------------------------------

# UI ------------
ui <- function(request) {
  page_navbar(
  title = app_title,
  footer = app_footer,
  id = "pages",
  fillable = FALSE,
  nav_spacer(),
  site_introduction,
  site_specification,
  site_about,
  !!!nav_items
)
}

# Server -----------
server <- function(input, output, session) {
  
  #observers to switch to and from the intro page
  observe({
    updateTabsetPanel(inputId = "pages", selected = "Specification")
  }) |> bindEvent(input$to_specification_form)
  
  observe({
    updateTabsetPanel(inputId = "pages", selected = "Introduction")
  }) |> bindEvent(input$to_introduction)
  
  #observer and functions to enable bookmarking and ensure it is in an accessible variable
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  url <- reactiveVal()
  
  onBookmarked(updateQueryString)
  onBookmarked(\(x) url(x))
  
  #output handler for PDF
  output$create_pdf <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_wearable_speclist_", input$general_project_name, ".pdf")
    },
    content = file_preparation("typst", url, input)
  )
  
  #output handler for word
  output$create_docx <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_wearable_speclist_", input$general_project_name, ".docx")
    },
    content = file_preparation("docx", url, input)
  )
  
}

shinyApp(ui, server, enableBookmarking = "url")