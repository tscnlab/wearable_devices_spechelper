software_specs <- tagList(
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
#   h4("Software Availability"),
#   checkboxInput("has_free_viewer", "Free viewer/export software provided", value = FALSE),
#   
#   h4("Export Formats"),
#   checkboxGroupInput("export_formats", "Supported export formats",
#                      choices = c("CSV", "Excel (XLSX)", "PDF/HTML report", "JSON", "Rectangular table"),
#                      selected = NULL),
#   
#   h4("Timekeeping & Timestamps"),
#   radioButtons("recording_tz", "Timestamp storage",
#                choices = c("Local time", "UTC", "No preference"),
#                selected = character(0)),
#   checkboxInput("handles_dst", "Handles daylight savings transitions", value = FALSE),
#   checkboxInput("retain_datetime", "Retains date/time after battery drain", value = FALSE),
#   
#   h4("Recording Features"),
#   checkboxInput("live_view", "Live data view capability", value = FALSE),
#   checkboxInput("exact_intervals", "Exact recording intervals (no drift)", value = FALSE),
#   selectizeInput("output_variables", "Variables recorded per interval",
#                  choices = c("Illuminance", "Melanopic EDI", "Movement", "Steps", "Temperature", "Battery level", "Other"),
#                  multiple = TRUE,
#                  options  = list(placeholder = "Select variable(s)")),
#   
#   h4("Automated Data Quality Checks"),
#   checkboxInput("wear_detection", "Automatic wear/non‑wear detection", value = FALSE),
#   checkboxInput("coverage_detection", "Automatic sensor coverage detection", value = FALSE),
#   checkboxInput("oob_detection", "Out‑of‑bounds measurement detection", value = FALSE),
#   
#   h4("Metadata & Storage"),
#   checkboxInput("metadata_storage", "Stores participant/session metadata with dataset", value = FALSE),
#   checkboxGroupInput("storage_mode", "Data storage during recording",
#                      choices = c("On‑device memory", "Cloud upload", "Smartphone companion app"),
#                      selected = NULL),
#   selectInput("cloud_location", "Preferred cloud server region",
#               choices = c("No preference", "EU (GDPR)", "US", "Other"), selected = "No preference")
# )

)