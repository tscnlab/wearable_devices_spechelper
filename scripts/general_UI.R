#app title
app_title <- 
h1(
    "Web-based specification tool for wearable", br(), "light loggers and optical radiation dosimeters"
)

#app footer

app_footer <- 
tags$footer(
  a(
    tags$div(style = "text-align:center;",
             tags$image(src = 'logo_banner2.png', width = "800px")
                 ),
    href = "https://www.melidos.eu",
    target = "_blank",
  ),
  br(),
  a(
    tags$div(style = "text-align:center;",
             tags$image(src = 'logo-mpi.png', width = "250px")),
    href = "https://www.kyb.tuebingen.mpg.de/en",
    target = "_blank",
  )
)

nav_items <- tagList(
  nav_item(
    a(
      span(tags$image(src = 'logo.png', width = "60px"), align = "center"),
      href = "https://github.com/tscnlab/LightLogR/",
      target = "_blank"
    )
  ),
nav_item(
  a(
    tags$image(src = "logo_with_text-01.png", width = "250px"),
    href = "https://www.tscnlab.org",
    target = "_blank"
  )
),
nav_item(
  input_dark_mode(id = "mode")
)
)