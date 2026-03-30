other_specs <- function() {
  tagList(
  
  h3("Budget"),
  layout_column_wrap(
    style = css(grid_template_columns = "6fr 3fr 6fr"),
    numericInput(
      "o_bvalue",
      "The maximum available budget (including shipping)",
      value = NULL,
      width = "100%"
    ),
    textInput(
      "o_bcurr",
      label = "Currency",
      value = NULL,
      width = "100%",
      placeholder = "Enter currency",
    ),
    selectInput(
      "o_bref",
      label = "Reference", 
      choices = c("per device", "total"),
      width = "100%"
    )
  ),
  checkboxInput("o_warr",
                width = "100%",
                label = tagList("Extended warranty/insurance required", icon("info-circle"))) |> 
    tooltip("Check if you require additional information from the manufacturer about extended warranty or insurance options"),
  selectizeInput("o_rent",
                     label = tagList("Acceptable options", icon("info-circle")),
                     width = "100%",
                 multiple = TRUE,
                     choices = c("One-time purchase", "Rental model", "Subscription model (software)"),
                     options  = list(placeholder = "What modes of device ownership would be acceptable?")) |> 
  tooltip(p("One-time purchase means full ownership of hardware and software."), 
          p("Rental model means you return the devices after an agreed upon time."), 
          p("Subscription model means that you buy/own the hardware, but that some software services - which need to be specified - are only available with an active subscription. This can include things like more in-depth analysis or cloud-storage.")),
  h3("Calibration"),  
  numericInput(
    inputId = "o_calib",
    label   = tagList("Minimum required calibration validity (months)", icon("info-circle")),
    value   = NULL,
    min     = 1,
    step    = 1,
    width = "50%"
  ) |> 
  tooltip(
    "How long (in months) the factory calibration should remain valid before recalibration is required. Suggestion: devices should require no recalibration with normal use under 24 months.",
    placement = "right",
    options   = list(container = "body")
  ),
  checkboxInput(
    inputId = "o_recal",
    label   = tagList("Recalibration information required", icon("info-circle")),
    width = "100%",
    value = FALSE) |> 
  tooltip(
    "Check if you require information from the manufacturer about recalibration (cost, procedure, modalities).",
    options   = list(container = "body")
  ),
  h3("Miscellaneous"),
  h5("Shipping"),
  dateInput(
    "o_deliv",
    "By which date will the devices be needed, i.e., delivered?",
    width = "50%",
    value = NA
  ),
  h5("Testing"),
  checkboxInput(
    "o_test",
    "Inquire options for a free test-device for internal assessment",
    value = FALSE,
    width = "100%"
  ),
  ## Accessories -------------------------------------------------------------
  h5("Accessories"),
  checkboxInput(
    inputId = "o_case",
    label   = tagList("Protective carrying case required", icon("info-circle")),
    value   = FALSE,
    width = "100%"
  ) |> 
  tooltip(
    id        = "protective_case",
    "Tick if the device must come with a light‑blocking, protective storage case.",
    placement = "right",
    options   = list(container = "body")
  ),
  
  ## Participant Materials ---------------------------------------------------
  h5("Participant Materials"),
  checkboxInput(
    inputId = "o_instr",
    label   = tagList("Wearer instructions required", icon("info-circle")),
    value   = FALSE
  ) |> 
  tooltip(
    "If checked, manufacturer must supply easy‑to‑understand instructions suitable for study participants on how to handle the device according to manufacturers specifications.",
    placement = "right",
    options   = list(container = "body")
  ),
  conditionalPanel(
    condition = "input.o_instr == true",
    textInput(
      inputId = "o_instr_lan",
      label   = "Instruction language(s)",
      width = "100%",
      placeholder = "Enter language(s)"
    )
  )
  )
  
}