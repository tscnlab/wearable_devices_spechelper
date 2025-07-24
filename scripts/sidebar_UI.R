#defining the sidebar
settings <- 
  function(){ 
  sidebar(
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
    bookmarkButton(id = "bookmark_button"),
    actionButton("to_example",
                 span(strong("Give me an example!")),
                 icon = icon("file-lines")),
    actionButton("to_introduction",
                 span(strong("Back to the introduction")),
                 icon = icon("arrow-left")),
    a(
      actionButton("to_research_guide",
                   span(strong("Go to RDA research guide")),
                   icon = icon("book")),
      href = "https://rda-wg-visualexperiencedata.github.io/ResearcherGuide/",
      target = "_blank"
    ),
    tags$head(
      tags$style(HTML("
      /* only modals that ALSO have class .wide */
      .modal-dialog {
        max-width: 90vw
      }
    ")))
  )
  }