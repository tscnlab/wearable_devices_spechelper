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
  )
)