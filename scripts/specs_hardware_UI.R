hardware_specs <- tagList(
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
      "hardware_device_depth",
      "Depth",
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
)