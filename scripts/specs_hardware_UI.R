hardware_specs <- function() {
  tagList(
  h3("Housing & design"),
  selectizeInput(
    "h_mount",
    "Select how the device needs to be mountable",
    choices = c("Wristband", "Necklace", "Spectacle-frame holder", "Clip", "Head-band", "Other"),
    multiple = TRUE,
    options  = list(placeholder = "Select required mounting options"),
    width = "100%"
  ),
  conditionalPanel(
    condition = "input.h_mount?.includes('Other')",
    textInput(
      inputId = "h_mount_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"
    )
  ),
  selectizeInput(
    "h_mat",
    "Housing material(s)",
    choices  = c("Plastic", "Aluminium", "Stainless steel", "Silicone", "Rubber", "Fabric", "Other"),
    multiple = TRUE,
    options  = list(placeholder = "Select acceptable surface materials"),
    width = "100%"
  ),
  conditionalPanel(
    condition = "input.h_mat?.includes('Other')",
    textInput(
      inputId = "h_mat_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"
    )
  ),
  div("What do you consider maximum dimensions?"),
  layout_column_wrap(
    sliderInput(
      "h_wid",
      "Width",
      min = 0.5, max = 10, value = 5, step = 0.5
    ),
    sliderInput(
      "h_dep",
      "Depth",
      min = 0.5, max = 10, value = 5, step = 0.5
    ),
    sliderInput(
      "h_hgt",
      "Height",
      min = 0.5, max = 10, value = 2.5, step = 0.5
    ),
  ),
  radioButtons(
    "h_unit",
    label = "Values in",
    inline = TRUE,
    choiceNames = c("centimeters (cm)", "inches (in)"),
    choiceValues = c("cm", "in"),
    width = "100%"
  ),
  numericInput("h_wgt", 
               "Maximum device weight (g)", 
               value = NULL, 
               min = 0, step = 1,
               width = "50%"),
  textAreaInput("h_notes", 
                "Special design considerations", 
                placeholder = "e.g., 'needs to be centered' for spectacle-mount",
                width = "100%"),
  h3("Measurement & sensors"),
  h5("Spectral characteristics"),
  sliderInput("h_spcRg",
              "Minimum and Maximum detectable wavelength",
              min = 100, 
              max = 1000,
              value = c(380, 780), step = 5,
              post = " nm",
              width = "100%"),
  sliderTextInput("h_specCh",
              "Minimum spectral resolution (in nm)",
              width = "60%",
              choices = c("<1", "1", "5", "10", "50", 
                          "100", ">100", "No spectral output required"),
              selected = "No spectral output required",
              grid = TRUE
              ),
    checkboxInput("h_ir", 
                "Requirement of IR sensing ≥ 1000nm", 
                value = FALSE,
                width = "100%"
                ),
  h5("Operating range"),
  sliderTextInput("h_illRg",
                  "Operating range (with specified error)",
                  choices = c("10⁻²", "10⁻¹", "1", "10", 
                              "10²", "10³", "10⁴", 
                              "10⁵"), 
                  selected = c("1", "10⁴"), grid = TRUE, post = " lx",
                  width = "100%"),
  numericInput("h_err", 
               "Acceptable measurement error within operating range (%)", 
               value = NULL, 
               width = "50%",
               min = 0, 
               max = 100, 
               step = 0.1),
  h5("Spatial detection"),
  checkboxInput("h_cos", 
                "Cosine‑corrected (lambertian) response", 
                width = "100%",
                value = FALSE),
  numericInput("h_fov", 
               "Field of view (°)", 
               value = NULL, 
               min = 0, 
               width = "50%",
               max = 180, 
               step = 1),
  h5("Other modalities"),
  selectizeInput(
    inputId = "h_mod",
    label   = "Other necessary measurement modalities",
    choices = c("Spectral power distribution (SPD)", 
                "Distance/Proximity", 
                "Accelerometer", 
                "Gyroscope", 
                "Spatial resolution",
                "Altimeter/Barometer",
                "Skin Temperature",
                "Ambient Temperature",
                "Skin Conductance",
                "Other"),
    selected  = NULL,
    multiple = TRUE,
    width = "100%",
    options   = list(placeholder = "Select if other modalities than light are required")
  ),
  conditionalPanel(
    condition = "input.h_mod?.includes('Other')",
    textInput(
      inputId = "h_mod_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"
    )
  ),
  numericInput("h_hz", 
               tagList("Highest required measurement sampling rate (Hz)", icon("info-circle")), 
               width = "50%",
               value = NULL, 
               min = 1, 
               step = 1)|> 
    tooltip(
      p("Sampling rate is how often measurements are taken (for example, once per second)."),
      p("Recording interval is how often those measurements are stored or logged."),
      p("If the sampling rate is higher than the recording interval, multiple samples are collected between recordings. In that case, the system must aggregate the samples (for example, by averaging or selecting representative values) before saving them."),
      placement = "right",
      options   = list(container = "body")
    ),
  h3("Battery & storage"),
  sliderTextInput(
    "h_bat",
    grid = TRUE,
    width = "100%",
    "How long does the device need to run on a single battery charge?", 
    choices = c("<1 day", "≥1 day", "≥1 week", "≥2 weeks", "≥1 month", ">1 year"), 
    selected = "≥2 weeks"
  ),
  layout_column_wrap(
  numericInput("h_stor_day", 
               "Required minimum on-device data storage capacity (days)", 
               width = "100%",
               value = NULL, 
               min = 1, 
               step = 1),
  sliderTextInput(
    "h_int",
    grid = TRUE,
    width = "100%",
    "What would be (closest to) the typical recording interval?", 
    choices = c("1s", "10s", "30s", "1min", "5min", "15min", "30min", "1h", "2h"), 
    selected = "10s"
  )),
  htmlOutput("storage_days", container = p),
  h3("Operating environment"),
  sliderInput("h_temp",
              tagList("Operating temperature (°C)", icon("info-circle")),
              min = -30, 
              max = 60,
              value = c(-10, 45), step = 5,
              post = " °C",
              width = "100%")|> 
    tooltip(
      p("-30 °C: Extremely cold — typical of Arctic winters or high-altitude mountain conditions."),
      p("0 °C to 10 °C: Cold outdoor environments, early winter mornings, or refrigerated storage."),
      p("20 °C to 35 °C: Normal everyday conditions — typical indoor and outdoor temperatures in most climates."),
      p("40 °C to 50 °C: Hot summer days in direct sunlight or inside a parked car."),
      p("60 °C and beyond: Very high heat — can occur e.g., in a sauna, on exposed surfaces, near machinery, or in desert conditions."),
      placement = "right",
      options   = list(container = "body")
    ),  
  sliderInput("h_hum",
              tagList("Operating humidity, relative (% RH)", icon("info-circle")),
              min = 0, 
              max = 100,
              value = c(20, 95), step = 5,
              post = " % RH",
              width = "100%") |> 
    tooltip(
      p("0–20%: Very dry air — common in heated indoor spaces or desert climates."),
      p("30–60%: Typical indoor and comfortable outdoor humidity range."),
      p("70–90%: Humid environments — tropical regions, bathrooms after showers, or misty mornings."),
      p("100%: Fully saturated air — fog, heavy rain, or condensation-prone conditions."),
      placement = "right",
      options   = list(container = "body")
    ),
  selectizeInput(
    inputId = "h_ip",
    label   = tagList("Ingress Protection (IP) rating", icon("info-circle")),
    choices = c("", "IPX0", "IPX1", "IPX2", "IPX3", "IPX4", "IP55", "IP64", "IP65", "IP66", "IP67", "IP68"),
    selected  = NULL,
    width = "100%",
    options   = list(placeholder = "Select IP rating (optional)")
  ) |> 
    tooltip(
      p("IPXY: X = solids (0–6), Y = water (0–8)."),
      p("Solids – 0 none, 1 >50 mm, 2 >12.5 mm, 3 >2.5 mm, 4 >1 mm, 5 dust‑protected, 6 dust‑tight."),
      p("Water – 0 none, 1 drips, 2 drips @15°, 3 spray, 4 splash, 5 jets, 6 powerful jets, 7 immersion ≤1 m, 8 immersion >1 m."),
      placement = "right",
      options   = list(container = "body")
    ),
  selectizeInput("h_sport", 
                 "Required device suitability for activities",
                 choices = c("Showering", "Biking", "Swimming", "Running", 
                             "Contact sports", "Climbing", "Other"),
                 selected = NULL,
                 options   = list(placeholder = "Select activities (optional)"),
                 multiple = TRUE,
                 width = "100%"),
  conditionalPanel(
    condition = "input.h_sport?.includes('Other')",
    textInput(
      inputId = "h_sport_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"
    )
  ),
  h3("Controls & indicators"),
  checkboxInput("h_event", 
                "Require event marker button", 
                value = FALSE,
                width = "100%"),  
  checkboxInput("h_recBtn", 
                "Require dedicated start/stop recording button", 
                value = FALSE,
                width = "100%"),  
  selectizeInput("h_statInd", 
                 "Required user feedback indicators (e.g. status light)",
                 choices = c("Charging", "Low battery", "Recording", "Error", "Active, but not recording", "Other"),
                 selected = NULL,
                 multiple = TRUE,
                 options   = list(placeholder = "Select required status indicators"),
                 width = "100%"),  
  conditionalPanel(
    condition = "input.h_statInd?.includes('Other')",
    textInput(
      inputId = "h_statInd_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"
    )
  ),
  h3("Connectivity & I/O"),
  selectizeInput("h_port", 
                 width = "100%",
                 "Acceptable connector(s) for charging/sync",
                 choices = c("USB‑C", 
                             "Micro‑USB", 
                             "Proprietary connector", 
                             "Docking station",
                             "Wireless charging",
                             "Other"),
                 options   = list(placeholder = "Accept possibilites for charging & sync"),
                 multiple = TRUE,
                 selected = NULL),
  conditionalPanel(
    condition = "input.h_port?.includes('Other')",
    textInput(
      inputId = "h_port_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"
    )
  ),
  selectizeInput("h_wlan", 
                 "Required wireless connectivity",
                 multiple = TRUE,
                 width = "100%",
                 options   = list(placeholder = "Select required wireless connectivity options"),
                 choices = c("Bluetooth", "Wi‑Fi", "Cellular radio", "NFC", "Other"),
                 selected = NULL),
  conditionalPanel(
    condition = "input.h_wlan?.includes('Other')",
    textInput(
      inputId = "h_wlan_o",
      label   = "Specify 'other' from previous entry",
      placeholder = "Enter text",
      width = "100%"
    )
  ),
  h3("Other hardware considerations"),
  textAreaInput("h_notesO", 
                "Other hardware considerations", 
                placeholder = "Add hardware requirements or important considerations that are not covered by the form",
                width = "100%"),
)
}