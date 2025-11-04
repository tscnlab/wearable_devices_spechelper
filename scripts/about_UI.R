site_about <- 
nav_panel(
  "About",
  card(
  p(
    "This application was designed by the ",
    a("Translational Sensory & Circadian Neuroscience Unit (MPS/TUM/TUMCREATE)", href = "https://www.tscnlab.org", target = "_blank"),
    "as part of the ",
    a("MeLiDos", href = "https://www.melidos.eu", target = "_blank"),
    " project. It is powered by Shiny for R. "
  ),
  p("The app version is 1.0.0, and was released 31 October 2025."),
  p("If you used this application, please cite as:"),
  tags$script(HTML("
  function copyAPA2() {
    navigator.clipboard.writeText(document.getElementById('apaText4').innerText);
  }
")),
  tags$script(HTML("
  function copyAPA() {
    navigator.clipboard.writeText(document.getElementById('apaText').innerText);
  }
")),
  fluidRow(
    column(
      width = 8,  # 6/12 = 50%
      actionButton("copy", "Software (click to copy)", onclick = "copyAPA2()"),
      p(
        tags$pre(id = "apaText4", class = "citation-pre", width = "100%",
                 "Zauner, J., Stefani, O., Biller, A. M., Guidolin, C., & Spitschan, M. (2025). 
Web-based specification tool for wearable light loggers and optical radiation dosimeters (Version 1.0.1) [Software]. 
https://doi.org/10.17617/1.04ga-fd22"
        ),
      ),
      actionButton("copy", "Code (click to copy)", onclick = "copyAPA()"),
      p(
        tags$pre(id = "apaText", class = "citation-pre", width = "100%",
                 "Zauner, J., Stefani, O., Biller, A. M., Guidolin, C., & Spitschan, M. (2025). 
Web-based specification tool for wearable light loggers and optical radiation dosimeters (Version 1.0.1) [Code]. 
Zenodo. https://doi.org/10.5281/zenodo.17487054"
        )
      )    )
  ),
  p(a("Link to Zenodo archive (DOI 10.5281/zenodo.17487054)",
      href="https://doi.org/10.5281/zenodo.17487054")),
  p(a("Link to GitHub repository",
      href="https://github.com/tscnlab/wearable_devices_spechelper"))
)
)
