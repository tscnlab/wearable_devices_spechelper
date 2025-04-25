#loading libraries
library(shiny)
library(bslib)
library(shinyWidgets)

#defining the sidebar
settings <- 
  sidebar(
    # fileInput("file", 
    #           span(bsicons::bs_icon("1-circle"), strong("Choose files")), 
    #           multiple = TRUE, 
    #           accept = c(".txt")
    # ),
    
    actionButton("create_pdf", 
                 span(strong("Create PDF")), 
                 icon = icon("file-pdf")
                 ),
    actionButton("to_research_guide",
                 span(strong("Go to RDA research guide")),
                 icon = icon("book")))

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
      open = "software",
      accordion_panel(
        title = h2("General information"),
        icon = bsicons::bs_icon("clipboard", size = "1.5em"),
        value = "general",
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
                      placeholder = "please provide a succinct description of the inded use case(s) for the wearables"
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
            choices = c("centimeters (cm)", "inches (in)"),
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
  #number of files
  output$n_files <- renderText({
    if(is.null(input$file)) return("no files provided")
    input$file |> nrow()
  })
  
  #filenames
  output$filenames <- renderText({
    input$file$name |>  paste0(collapse = ", ")
  })
  
  #pattern to capture Ids
  output$pattern <- renderText({
    if(is.null(input$file)) return("no files provided")
    input$file$name |> 
      stringr::str_extract_all("(S\\d{3}_[wch])", simplify = TRUE) |> 
      paste0(collapse = ", ")
  })
  
  #change the filenames to the original name
  new_names <- reactive({
    req(input$file)
    #renaming the temp-files to their old filename
    new_names <- paste0(dirname(input$file$datapath), "/", input$file$name)
    file.rename(input$file$datapath, new_names)
    new_names
  }) |> bindEvent(input$file)
  
  # import and import message
  data <- reactive({
    # req(input$file)
    #actual import
    data <- import$ActLumus(new_names(), tz = input$tz, auto.id = "(S\\d{3}_[wch])",
                            dst_adjustment = "dst_jumps" %in% input$options,
                            # remove_duplicates = "remove_duplicates" %in% input$options,
                            auto.plot = FALSE, print_n = Inf) 
    data
  }) |> bindEvent(input$import)
  
  observe({ 
    showNotification( 
      paste("Import is in progress. Should the app freeze and grey out, if the import is not successful. A message will be shown upon successfull import."),
      type = "default", 
      duration = 5 
    ) 
  }) |> 
    bindEvent(input$import)
  
  observe({ 
    nav_select("main", selected = "Analysis")
  }) |> 
    bindEvent(input$to_analysis)
  
  #outputs p1
  output$import_msg <- renderPrint({
    invisible(data())
  })
  
  observe({
    # req(data())
    showModal(
      modalDialog(
        title = icon("check", style = "font-size: 60px;"),
        easy_close = TRUE,
        "Import seems to have been successful. Please check the import message and overview plot and continue to the analysis tab if satisfied."
      ),
    )
  }) |>
    bindEvent(data())
  
  
  # output$import_table <- renderDT({
  #   req(data())
  #   data()
  # })
  
  output$plot_overview <- renderPlot({
    req(data())
    data() |> gg_overview()
  })
  
  
  #------------ page Analysis
  
  #create a wider data frame
  data2 <- reactive({
    req(data())
    data() |> 
      tidyr::separate_wider_delim("Id", "_" , names = c("Id", "Position")) |> 
      dplyr::mutate(Position = factor(Position, levels = c("w", "c", "h"), labels = c("wrist", "chest", "glasses")),
             Id = factor(Id)) |> 
      dplyr::group_by(Id, Position)
  })
  
  #epoch
  output$epoch <- renderText({
    req(data2())
    data2() |> dominant_epoch() |> dplyr::pull(dominant.epoch) |> unique() |> as.duration() |> paste0(collapse = ", ")
  })
  
  #gaps
  output$gaps <- renderText({
    req(data2())
    gaps <- 
      data2() |> gap_finder(gap.data = TRUE) |> summarize(last(gap.id)) |> pull(3) |> median()
    if(is.na(gaps)) {
      gaps <- "No gaps found"
    }
    gaps
  })
  
  #plot output
  output$plot_ggdays <- renderPlot({
    req(data2())
    data2() |> 
      gg_day(geom = "ribbon", aes_fill = Position, aes_col = Position, 
             group = interaction(Id, Position), alpha = 0.25, linewidth = 0.1)
    
  })
  
  
}

shinyApp(ui, server)