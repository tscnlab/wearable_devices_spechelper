file_preparation <- function(type = c("typst", "docx"), url, input) {
  
  type <- match.arg(type)
  
function(file) {
  url <- url()
  input_list <- input |> reactiveValuesToList()
  saveRDS(input_list, file = "input_list.rds")
  quarto::quarto_render(
    input = "report_template.qmd",
    output_format = type,
    execute_params = list(
      project_name = input_list$g_pname,
      bookmark = url
    )
  )
  # copy the quarto generated file to `file` argument.
  generated_file_name <- paste0("report_template.", ifelse(type == "docx", "docx", "pdf"))
  file.copy(generated_file_name, file)
  #remove the generated RDS file
  # file.remove("input_list.rds")
}
}
