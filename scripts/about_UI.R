site_about <- 
nav_panel(
  "About",
  card(
  p(
    "This application was designed by the ",
    a("TSCN Unit", href = "https://www.tscnlab.org", target = "_blank"),
    "as part of the ",
    a("MeLiDos", href = "https://www.melidos.eu", target = "_blank"),
    " project. It is powered by Shiny for R. "
  ),
  p("The app version is 1.0.0, and was released 31 October 2025."),
  p("If you used this application, please cite as:"),
  tags$script(HTML("
  function copyAPA() {
    navigator.clipboard.writeText(document.getElementById('apaText').innerText);
  }
")),
  fluidRow(
    column(
      width = 8,  # 6/12 = 50%
      div(
        tags$pre(id = "apaText", class = "citation-pre", width = "50%",
                 "Zauner, J., Stefani, O., Biller, A. M., Guidolin, C., & Spitschan, M. (2025). 
Wearable light logger and optical radiation dosimeter specification tool (Version 1.0.0) [Computer software]. 
Zenodo. https://doi.org/10.5281/zenodo.17487054
Available at https://tscnlab-wearable-devices-specification.share.connect.posit.cloud"
                 
        ),
        actionButton("copy", "Copy", onclick = "copyAPA()")
      )    )
  ),
  p(a("Link to Zenodo archive (DOI 10.5281/zenodo.17487054)",
      href="https://doi.org/10.5281/zenodo.17487054")),
  p(a("Link to GitHub repository",
      href="https://github.com/tscnlab/wearable_devices_spechelper"))
)
)
