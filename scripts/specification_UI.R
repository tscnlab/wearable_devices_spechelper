#load `settings`, containing the sidebar info
source("scripts/sidebar_UI.R")
source("scripts/specs_general_UI.R")
source("scripts/specs_hardware_UI.R")
source("scripts/specs_software_UI.R")
source("scripts/specs_other_UI.R")

site_specification <- 
nav_panel(
  "Specification",
  layout_sidebar( sidebar = settings, 
                  accordion(
                    multiple = FALSE,
                    open = "general",
                    accordion_panel(
                      title = h2("General information"),
                      icon = bsicons::bs_icon("clipboard", size = "1.5em"),
                      value = "general",
                      !!!general_specs
                    ),
                    accordion_panel(
                      title = h2("Hardware requirements"),
                      icon = bsicons::bs_icon("motherboard", size = "1.5em"),
                      value = "hardware",
                      !!!hardware_specs
                    ),
                    accordion_panel(
                      title = h2("Data requirements"),
                      icon = bsicons::bs_icon("code-slash", size = "1.5em"),
                      value = "software",
                      !!!software_specs
                    ),
                    accordion_panel(
                      title = h2("Other requirements"),
                      icon = bsicons::bs_icon("clipboard-plus", size = "1.5em"),
                      value = "other",
                      !!!other_specs
                    )
                  )
  )
)
