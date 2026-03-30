general_specs <- function() {
  tagList(
  textInput(
    "g_pname",
    label = "Project name",
    placeholder = "Short project name",
    width = "100%"
  ),
  selectizeInput(
    "g_stype",
    "Select the type of study",
    choices = c("Observational", "Interventional", "Case-control", "Cohort", "Other"),
    multiple = TRUE,
    width = "100%",
    options  = list(placeholder = "Select the type of study you require the devices for")
  ),
  conditionalPanel(
  condition = "input.g_stype?.includes('Other')",
    textInput(
      inputId = "g_stype_o",
      label   = "Specify 'other' from previous entry",
      width = "100%",
      placeholder = "Enter text"
    )
  ),
  textInput(
    "g_country",
    label = "Country of deployment & shipment",
    placeholder = "Enter the (initial) country of deployment & shipment",
    width = "100%"
  ),
  textAreaInput("g_s_desc",
                label = "Description of the (main) use case",
                width = "100%",
                rows = 3,
                placeholder = "Please provide a succinct description of the intended use case(s) for the wearables. Include specifics that might affect the choice of wearables, such as participant age, health conditions, activity level, etc. (Long text will break bookmarking in some browsers)"
  ),
  numericInput("g_d_num",
               "How many devices do you require?",
               value = NULL, min = 1, step = 1,
               width = "50%"
  )
)
}