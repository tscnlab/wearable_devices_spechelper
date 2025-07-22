#defining the sidebar
settings <- 
  sidebar(
    open = FALSE,
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
    a(
      actionButton("to_research_guide",
                   span(strong("Go to RDA research guide")),
                   icon = icon("book")),
      href = "https://rda-wg-visualexperiencedata.github.io/ResearcherGuide/",
      target = "_blank"
    ),
    actionButton("to_introduction",
                 span(strong("Back to the introduction")),
                 icon = icon("arrow-left"))
  )
