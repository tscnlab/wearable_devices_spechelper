#defining the sidebar
settings <- 
  function(){ 
  sidebar(
    strong("Navigation"),
    actionButton("to_introduction",
                 span(strong("Introduction")),
                 icon = icon("circle-info")),
    actionButton("to_specification",
                 span(strong("Specification")),
                 icon = icon("passport")),
    actionButton("to_about",
                 span(strong("About")),
                 icon = icon("circle-question")),
    a(
      actionButton("to_research_guide",
                   span(strong("Go to RDA research guide")),
                   icon = icon("book")),
      href = "https://rda-wg-visualexperiencedata.github.io/ResearcherGuide/",
      target = "_blank"
    ),
    conditionalPanel(
      condition = "input.pages == 'Specification'",
      p(style = 'border-bottom: 1px solid'),
      strong("Downloads"),
      # open = FALSE,
      downloadButton("create_docx", 
                     span(strong("Create & Download Word")), 
                     icon = icon("file-word"),
                     class = "btn-success btn-lg"
      ),
      downloadButton("create_pdf", 
                     span(strong("Create & Download PDF")), 
                     icon = icon("file-pdf")
      ),
      p(style = 'border-bottom: 1px solid'),
      strong("Further options"),
      bookmarkButton(id = "bookmark_button",
                     label = strong("Bookmark...")
      ),
      actionButton("to_example",
                   span(strong("Give me an example!")),
                   icon = icon("file-lines")),
      actionButton("reset_app", "Start fresh", icon = icon("undo")),
      actionButton("to_cite",
                   span(strong("Cite me")),
                   icon = icon("quote-right")),
      tags$head(
        tags$style(HTML("
      /* only modals that ALSO have class .wide */
      .modal-dialog {
        max-width: 90vw
      }
    ")))
    )
  )
  }