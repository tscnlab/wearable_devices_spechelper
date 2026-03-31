#loading libraries
library(shiny)
library(bslib)
library(shinyWidgets)
library(yaml)
library(lubridate)

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

#load string for example
source("scripts/query_string.R", local = TRUE)

# --- Shiny app -------------------------------------------

# UI ------------
ui <- function(request) {
  page_navbar(
    # selected = "Specification",
  title = tagList(app_title,   
  tags$style(HTML("
    /* Let the brand text wrap and break long words if needed */
    .navbar .navbar-brand {
      min-width: 0;              /* allow flexbox to shrink it */
      white-space: normal;       /* permit wrapping */
      overflow-wrap: anywhere;   /* break long words/URLs */
      line-height: 1.1;
    }
  "))),
  footer = app_footer,
  fillable = FALSE,
  nav_spacer(),
  # site_introduction,
  # site_introduction,
  nav_panel_hidden(
    "General",
    card(
      layout_sidebar(
        sidebar = settings(),
        navset_hidden(
          id = "pages",
          site_introduction,
          site_specification(),
          site_about 
        )
      )
    )
  ),
  # site_about,
  !!!nav_items,
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
  
  observe({
    updateTabsetPanel(inputId = "pages", selected = "About")
  }) |> bindEvent(input$to_cite, input$to_about, ignoreInit = TRUE)
  
  observe({
    updateTabsetPanel(inputId = "pages", selected = "Specification")
  }) |> bindEvent(input$to_specification)
  
  observe({
    active_navigation_button <- switch(
      input$pages,
      "Introduction" = "to_introduction",
      "Specification" = "to_specification",
      "About" = "to_about",
      "to_specification"
    )

    session$sendCustomMessage("setActiveNavButton", active_navigation_button)
  })
  
  #observer and functions to enable bookmarking and ensure it is in an accessible variable
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  observeEvent(input$reset_app, {
    updateQueryString("?_inputs_&pages=%22Specification%22")
    session$reload()  # restart the session, clearing all inputs
  })
  
  observe({
    # updateQueryString(query_string, mode = "push")
    cd <- session$clientData
    example_url <- 
      paste0(
        cd$url_protocol,            # "http:" or "https:"
        "//",
        cd$url_hostname,            # "my‑server.com"
        if (nzchar(cd$url_port) && cd$url_port != "80") 
          paste0(":", cd$url_port)  # ":3838" (blank for default ports)
        else "",
        cd$url_pathname,            # "/myapp/"
        query_string,              # "?a=1&b=two"
        cd$url_hash_initial         # "#section"
      )
    showModal(urlModal(example_url, title = "Copy this link into a new browser window to see an example"))
    # print(example_url)
  }) |> bindEvent(input$to_example)
  
  observe({
    showBookmarkUrlModal(url())
  }) |> bindEvent(input$bookmark_button)
  
  url <- reactiveVal()
  
  setBookmarkExclude(c("to_example", "to_introduction", "bookmark_button",
                       "create_docx", "create_pdf", "to_research_guide",
                       "reset_app"))
  # onBookmarked(updateQueryString)
  onBookmarked(\(x) url(x))
  
  #output handler for PDF
  output$create_pdf <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_wearable_speclist_", input$g_pname, ".pdf")
    },
    content = file_preparation("typst", url, input)
  )
  
  #output handler for word
  output$create_docx <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_wearable_speclist_", input$g_pname, ".docx")
    },
    content = file_preparation("docx", url, input)
  )
  
  output$create_docx2 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_wearable_speclist_", input$g_pname, ".docx")
    },
    content = file_preparation("docx", url, input)
  )
  
  output$storage_days <- renderText({
    req(input$h_stor_day, input$h_int)
    obs_number <-
    input$h_stor_day * 24*60*60/
      as.numeric(as.duration(input$h_int))
    (paste("Storage capacity required for a minimum of",
               strong(obs_number) , "observations"))
    })
  
}

shinyApp(ui, server, enableBookmarking = "url")