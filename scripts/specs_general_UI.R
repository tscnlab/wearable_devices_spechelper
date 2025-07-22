general_specs <- tagList(
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
)