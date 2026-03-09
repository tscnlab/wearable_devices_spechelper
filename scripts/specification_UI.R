#load `settings`, containing the sidebar info
source("scripts/sidebar_UI.R")
source("scripts/specs_general_UI.R")
source("scripts/specs_hardware_UI.R")
source("scripts/specs_software_UI.R")
source("scripts/specs_other_UI.R")

site_specification <- function(){
nav_panel(
  "Specification",
  layout_sidebar( sidebar = settings(), 
                  div(
                    class = "specs-accordion-hint",
                    icon("hand-pointer"),
                    strong(" Details:"),
                    " click a section header below to expand and edit requirements."
                  ),
                  accordion(
                    multiple = FALSE,
                    # open = "software",
                    accordion_panel(
                      title = tagList(h2("General information"), span(class = "details-pill", "Click to open")),
                      icon = bsicons::bs_icon("clipboard", size = "1.5em"),
                      value = "general",
                      general_specs()
                    ),
                    accordion_panel(
                      title = tagList(h2("Hardware requirements"), span(class = "details-pill", "Click to open")),
                      icon = bsicons::bs_icon("motherboard", size = "1.5em"),
                      value = "hardware",
                      hardware_specs()
                    ),
                    accordion_panel(
                      title = tagList(h2("Data requirements"), span(class = "details-pill", "Click to open")),
                      icon = bsicons::bs_icon("code-slash", size = "1.5em"),
                      value = "software",
                      software_specs()
                    ),
                    accordion_panel(
                      title = tagList(h2("Other requirements"), span(class = "details-pill", "Click to open")),
                      icon = bsicons::bs_icon("clipboard-plus", size = "1.5em"),
                      value = "other",
                      other_specs()
                    )
                  ),
                  downloadButton("create_docx2", 
                                 span(strong("Create & Download Word")), 
                                 icon = icon("file-word"),
                                 class = "btn-success btn-lg"
                  )
  )
)
}
