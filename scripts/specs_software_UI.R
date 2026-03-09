software_specs <- function() {
  tagList(
  h3("Software"),
  selectizeInput(
    "s_opsys",
    "Which operating systems must provide syncing & export options for the device",
    choices = c("Windows", "MacOS", "Linux", "iOS", "Android"),
    multiple = TRUE,
    options  = list(placeholder = "Select operating systems you use/require in the study"),
    width = "100%"
  ),
  selectizeInput("s_fun", 
                     "Requirement for software functions",
                     choices = c("Overview (e.g., duration, completeness, intervall)", 
                                 "Display recorded time series", 
                                 "Calculate metrics", 
                                 "Pre-set recording times", 
                                 "Live data view (during recording)",
                                 "Other"),
                options  = list(placeholder = "Select which software functions are required"),
                width = "100%",
                multiple = TRUE,
                selected = NULL),
  conditionalPanel(
    condition = "input.s_fun?.includes('Other')",
    textInput(
      inputId = "s_fun_o",
      label   = "Specify 'other' from previous entry",
      width = "100%",
      placeholder = "Enter text"
    )
  ),
  conditionalPanel(
    condition = "input.s_fun?.includes('Calculate metrics')",
    textInput(
      inputId = "s_fun_met",
      label   = "Specify which metrics need to be calculated by the software",
      width = "100%",
      placeholder = "Enter text"
    )
  ),
  h3("Exported data"),
  selectizeInput("s_exp",
                 label = "Required export format options",
                 multiple = TRUE,
                 width = "100%",
                 choices = c("CSV", "Excel (XLS/XLSX)", "PDF/HTML report", "JSON", "Other"),
                 options  = list(placeholder = "Select export formats you use/require in the study"),
                 selected = NULL),
  conditionalPanel(
    condition = "input.s_exp?.includes('Other')",
    textInput(
      inputId = "s_exp_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"
    )
  ),
  selectizeInput("s_vars", 
                 "Required export variables (besides timestamp)",
                 choices = c("Illuminance", "Melanopic EDI", 
                             "Other alpha-opic values", 
                             "Activity", "Steps", "Temperature", "Battery level",
                             "Sensor counts", 
                             "Gain levels", "Integration time", "Device angle",
                             "Other"),
                 multiple = TRUE,
                 width = "100%",
                 options  = list(placeholder = "Select variable(s)")),
  conditionalPanel(
    condition = "input.s_vars?.includes('Other')",
    textInput(
      inputId = "s_vars_o",
      label   = "Specify 'other' from previous entry",
      width = "100%",
      placeholder = "Enter text"
    )
  ),
  selectizeInput("s_tz", "Timestamp format",
                 choices = c("", "UTC", "Local time + Offset (ISO 8601)", "Local time"),
                 width = "100%",
                 options  = list(placeholder = "Select timestamp format"),
                 selected = NULL),
  checkboxInput("s_loc", 
                 "Export format must be locale-independent",
                width = "100%",
                 value = TRUE) |> 
    tooltip(
      "This means that the exported file does not depend on the locale settings of a device or computer. This includes aspects such as date and time formats, choice of delimiters, and decimal separators. It is generally recommended to keep the file independent of locale settings, since varying formats can make it harder to import data into programs or share it reliably between researchers."
    ),
  checkboxInput("s_meta", 
                "Option to store participant/session metadata with dataset", 
                width = "100%",
                value = FALSE),
  h3("Timestamps & recording interval"),
  sliderTextInput(
    "s_interv",
    grid = TRUE,
    width = "100%",
    "What is the minimum recording interval you require?",
    selected = "≤ 30 seconds",
    choices = c("<1 second", "≤ 10 seconds", "≤ 30 seconds",  "≤ 60 seconds", "≤ 5 minutes", "≤ 15 minutes", "≤ 30 minutes", "≤ 1 hour", "≤ 1 day", "≥ 1 day"),
  ),
  radioButtons(
    "s_flex",
    label = "How flexible does the recording interval need to be (at setup)?",
    choices = c("One setting (in the specified range) is sufficient",
                "Multiple options are required to allow for coarser intervals",
                "Recording interval needs to be freely adjustable"), 
    selected = 0,
    width = "100%"
  ),
  checkboxInput("s_exact", 
                "Exact recording intervals (no deviations from set recording interval)", 
                width = "100%",
                value = FALSE),
  checkboxInput("s_dst", 
                "Requirement to handle daylight savings transitions correctly for timestamps", 
                width = "100%",
                value = FALSE),
  checkboxInput("s_retaindt", 
                tagList("Requirement to retain correct date & time after battery drain", icon("info-circle")),
                width = "100%",
                value = FALSE) |> 
    tooltip("Some devices loose the datetime when the battery is drained. When they are charged and restart (without syncing to a PC), they have an incorrect timestamp."),
  h3("Automated checks and detections"),
  selectizeInput("s_auto",
                     label = "Requirement for automated detection & flagging of:",
                choices = c("Wear/non‑wear", "Sensor coverage",
                            "Out-of-bounds measurements", "Sleep/Wake", "Other"),
                multiple = TRUE,
                options  = list(placeholder = "Select where automated detection is required"),
                selected = NULL,
                width = "100%"),
  conditionalPanel(
    condition = "input.s_auto?.includes('Other')",
    textInput(
      inputId = "s_auto_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"    
      )
  ),
  h3("Storage"),
  checkboxGroupInput("s_stor", 
                     tagList("Acceptable locations of data storage (unchecked locations are not acceptable)", icon("info-circle")),
                     choices = c("On‑device memory", "Cloud upload", "Smartphone companion app"),
                     width = "100%",
                     selected = "On‑device memory") |> 
    tooltip("Select where data can be stored until it is exported by the researcher"),
  conditionalPanel(
    condition = "input.s_stor?.includes('Cloud upload')",
    textInput(
      inputId = "s_stor_loc",
      label   = "Optionally set a requirement for cloud server location",
      placeholder = "Enter locations",
      width = "100%",
    )
  )

)
}