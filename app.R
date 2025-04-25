#loading libraries
library(shiny)
library(bslib)
library(shinyWidgets)
library(yaml)

#defining the sidebar
settings <- 
  sidebar(
    # fileInput("file", 
    #           span(bsicons::bs_icon("1-circle"), strong("Choose files")), 
    #           multiple = TRUE, 
    #           accept = c(".txt")
    # ),
    
    downloadButton("create_pdf", 
                 span(strong("Create & Download PDF")), 
                 icon = icon("file-pdf")
                 ),
    a(
    actionButton("to_research_guide",
                 span(strong("Go to RDA research guide")),
                 icon = icon("book")),
    href = "https://rda-wg-visualexperiencedata.github.io/ResearcherGuide/",
    target = "_blank"
    )
  )

#defining the main body
cards <- list(
  # value_box(
  #   "Number of files", 
  #   value = textOutput("n_files"),
  #   showcase = bsicons::bs_icon("journals"),
  #   theme = "primary",
  #   textOutput("filenames")
  # ),
  # value_box(
  #   span(
  #     "Extracted Ids from filenames", 
  #     icon("question-circle") |> 
  #       popover("Ids will be named based on the filename with the following pattern: 'S\\d{3}_[wch]', i.e. an S followed by a three-digit number, and underscore w c or h, standing for wrist, chest, or head/glasses, respectively.")
  #   ),
  #   value = textOutput("pattern"),
  #   showcase = bsicons::bs_icon("search"),
  #   theme = "secondary"
  # ) 
)

# UI ------------
ui <- page_navbar(
  title = h1(
    span(
      "Wearable light logger and optical radiation dosimeter",
      br(),
      "Specification tool"
    )
  ),
  sidebar = settings,
  footer = tags$footer(
    a(
      tags$image(src = 'logo_banner2.png', width = "100%"),
      href = "https://www.melidos.eu",
      target = "_blank"
    )
  ),
  id = "main",
  # bg = "dar",
  # selected = "Main",
  # gap = "2px",
  fillable = FALSE,
  nav_spacer(),
  nav_panel(
    "Main",
    accordion(
      multiple = FALSE,
      open = "general",
      accordion_panel(
        title = h2("General information"),
        icon = bsicons::bs_icon("clipboard", size = "1.5em"),
        value = "general",
        textInput(
          "general_project_name",
          label = "Project name",
          placeholder = "short project name, used for the PDF filename",
          width = "100%"
        ),
        selectInput(
          "general_study_type",
          "Select the type of study",
          choices = c("observational", "interventional", "case-control", "cohort"),
          multiple = TRUE,
          width = "100%"
        ),
        textAreaInput("general_study_description", 
                      label = "Description of the (main) use case", 
                      width = "100%", 
                      rows = 4,
                      placeholder = "please provide a succinct description of the intended use case(s) for the wearables"
                      ),
        numericInput("general_device_number",
                     "How many devices do you require?", 
                     value = NULL, min = 1, step = 1,
                     width = "100%"
        )
      ),
      accordion_panel(
        title = h2("Hardware requirements"),
        icon = bsicons::bs_icon("motherboard", size = "1.5em"),
        value = "hardware",
        selectInput(
          "hardware_device_mounting",
          "Select how the device needs to be mountable",
          choices = c("wristband", "necklace", "spectacle-frame holder", "clip", "head-band"),
          multiple = TRUE,
          width = "100%"
        ),
        h3("Device size"),
        div("What do you consider maximum dimensions?"),
        layout_column_wrap(
          sliderInput(
            "hardware_device_width",
            "Width",
            min = 0.5, max = 10, value = 5, step = 0.5
          ),
          sliderInput(
            "hardware_device_breadth",
            "Breadth",
            min = 0.5, max = 10, value = 5, step = 0.5
          ),
          sliderInput(
            "hardware_device_height",
            "Height",
            min = 0.5, max = 10, value = 2, step = 0.5
          ),
        ),
          radioButtons(
            "hardware_device_size_unit",
            label = "Values in",
            inline = TRUE,
            choiceNames = c("centimeters (cm)", "inches (in)"),
            choiceValues = c("cm", "in"),
            width = "100%"
          ),
        h3("Battery"),
        sliderTextInput(
          "hardware_battery_capacity",
          grid = TRUE,
          width = "100%",
          "How long do you need the device to run on a single battery charge, given the recording/sampling interval you specified?", 
          choices = c("<1 day", "≥1 day", "≥1 week", "≥2 weeks", "≥1 month", ">1 year"), selected = "≥2 weeks"
        )
      ),
      accordion_panel(
        title = h2("Data requirements"),
        icon = bsicons::bs_icon("code-slash", size = "1.5em"),
        value = "software",
        sliderTextInput(
          "data_recording_interval",
          grid = TRUE,
          width = "100%",
          "What is the minimum recording interval you require?",
          selected = "≤ 30 seconds",
          choices = c("<1 second", "≤ 10 seconds", "≤ 30 seconds",  "≤ 60 seconds", "≤ 5 minutes", "≤ 15 minutes", "≤ 30 minutes", "≤ 1 hour", "≤ 1 day", "≥ 1 day"),
        ),
        radioButtons(
          "data_recording_flexibility",
          label = "How flexible does the recording interval need to be (at setup)?",
          choices = c("One setting (in the specified range) is sufficient",
                      "Multiple options are required to allow for coarser intervals",
                      "Recording interval needs to be freely adjustable"), 
          selected = 0,
          width = "100%"
        ),
      ),
      accordion_panel(
        title = h2("Other requirements"),
        icon = bsicons::bs_icon("clipboard-plus", size = "1.5em"),
        value = "other",
        
        h3("Budget"),
        layout_column_wrap(
          style = css(grid_template_columns = "6fr 3fr 6fr"),
          numericInput(
            "other_budget_value",
            "The maximum available budget",
            value = NULL,
            width = "100%"
          ),
          textInput(
            "other_budget_currency",
            label = "Currency",
            value = NULL,
            width = "100%"
          ),
          selectInput(
            "other_budget_reference",
            label = "Reference", 
            choices = c("per device", "total"),
            width = "100%"
          )
        )
      )
    )
  ),
  nav_panel(
    "About",
    p(
      "This application was designed by the ",
      a("TSCN Unit", href = "https://www.tscnlab.org", target = "_blank"),
      "as part of the ",
      a("MeLiDos", href = "https://www.melidos.eu", target = "_blank"),
      " project. It is powered by Shinylive for R. "
    )
  ),
  nav_item(
    a(
      span(tags$image(src = 'logo.png', width = "60px"), align = "center"),
      href = "https://github.com/tscnlab/LightLogR/",
      target = "_blank"
    )
  ),
  nav_item(
    a(
      tags$image(src = "logo_with_text-01.png", width = "250px"),
      href = "https://www.tscnlab.org",
      target = "_blank"
    )
  ),
  nav_item(
    input_dark_mode(id = "mode")
    )
)


# Server -----------
server <- function(input, output) {
  
  output$create_pdf <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_wearable_speclist_", input$general_project_name, ".pdf")
    },
    content = function(file) {
      input_list <- input |> reactiveValuesToList()
      saveRDS(input_list, file = "input_list.rds")
      quarto::quarto_render(
        input = "report_template.qmd",
        execute_params = list(
          project_name = input_list$general_project_name
        )
      )
      # copy the quarto generated file to `file` argument.
      generated_file_name <- "report_template.pdf"
      file.copy(generated_file_name, file)
      #remove the generated RDS file
      file.remove("input_list.rds")
    }
  )
  
}

shinyApp(ui, server)