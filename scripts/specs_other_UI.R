other_specs <- tagList(
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
  ),
  h4("Calibration"),
  numericInput(
    inputId = "calibration_valid_months",
    label   = "Calibration validity (months)",
    value   = NULL,
    min     = 1,
    step    = 1
  ),
  # bsTooltip(
  #   id        = "calibration_valid_months",
  #   title     = "How long (in months) the factory calibration should remain valid before recalibration is required.",
  #   placement = "right",
  #   options   = list(container = "body")
  # ),
  
  radioButtons(
    inputId = "recalibration_method",
    label   = "Recalibration option/cost",
    choices = c(
      "No recalibration needed",
      "Manufacturer service (paid)",
      "Manufacturer service (free)",
      "User recalibrates"
    ),
    selected = character(0)
  ),
  # bsTooltip(
  #   id        = "recalibration_method",
  #   title     = "Select the acceptable recalibration process and whether an extra fee is acceptable.",
  #   placement = "right",
  #   options   = list(container = "body")
  # ),
  ## Accessories -------------------------------------------------------------
  h4("Accessories"),
  checkboxInput(
    inputId = "protective_case",
    label   = "Protective carrying case required",
    value   = FALSE
  ),
  # bsTooltip(
  #   id        = "protective_case",
  #   title     = "Tick if the device must come with a light‑blocking, protective storage case.",
  #   placement = "right",
  #   options   = list(container = "body")
  # ),
  
  ## Participant Materials ---------------------------------------------------
  h4("Participant Materials"),
  checkboxInput(
    inputId = "wearer_instructions",
    label   = "Provide wearer instructions",
    value   = FALSE
  ),
  # bsTooltip(
  #   id        = "wearer_instructions",
  #   title     = "If checked, manufacturer must supply easy‑to‑understand instructions suitable for study participants.",
  #   placement = "right",
  #   options   = list(container = "body")
  # ),
  conditionalPanel(
    condition = "input.wearer_instructions == true",
    selectizeInput(
      inputId = "instruction_languages",
      label   = "Instruction language(s)",
      choices = c("English", "German", "Spanish", "French", "Italian", "Chinese", "Japanese", "Other"),
      multiple = TRUE,
      options  = list(placeholder = "Select language(s)")
    )
  )
  )