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
  )
)