# --- Intro Page UI component -------------------------------------------------
introPage <- tagList(
  
  h3("Background"),
  p("Wearable light loggers are increasingly used to study how light exposure affects human physiology and behaviour. For example, researchers examine how light influences circadian rhythm entrainment (synchronisation of the body’s internal clock) and contributes to eye growth issues like myopia (nearsightedness). This guide is designed to help researchers make informed decisions when choosing a wearable light logger by outlining key device features and typical use cases in an accessible way."),
  
  h3("Feature Dimensions"),
  p("A wearable light logger is defined by many features – including size, how it can be worn (attachments or accessories), logging duration, battery life, what light quantities it measures, and its accuracy. This guide highlights two primary feature dimensions crucial for device selection: long-term usability and measurement fidelity and accuracy."),
  
  h4("Long‑Term Usability"),
  p("Long‑term usability refers to how well a device can be worn in real‑world conditions over extended periods. This dimension covers wearer comfort (e.g. how easily the logger attaches to clothing or accessories, whether it is inconspicuous, its size and weight) and the device’s ability to reliably collect data over time (battery life, data storage capacity, hardware stability, etc.). While each of these factors can be considered independently, together they determine whether a light logger can be practically used for days, weeks, or longer in a person’s daily life."),
  
  h4("Fidelity and Accuracy"),
  p("Fidelity and accuracy describe the quality of the light measurements a logger provides. Important contributors to fidelity include the device’s spectral sensitivity (how well it matches standard α‑opic quantities – light metrics tied to human photoreceptor responses) and the range of light intensities it can record (capturing environments from very dim to very bright). The sensor’s directional response (how its sensitivity changes with the angle of incoming light) is another key factor. Additionally, the proximity of the sensor to the eye (the corneal plane) greatly affects fidelity – a sensor positioned at eye level will measure light similarly to what enters the eye, closely approximating the retinal irradiance that drives non‑visual biological responses to light."),
  
  h3("Use Cases"),
  p("The optimal choice of a wearable light logger depends on the study design and research question. We consider three typical use cases, defined by deployment duration, that illustrate a range of scenarios: short‑term, medium‑term, and long‑term measurements."),
  
  h4("Short‑Term Use (< 24 hours): Compliance and Stimulus Control"),
  p("For short experimental sessions (less than 24 hours), a wearable light logger can verify that participants receive the intended light exposure. When an experiment involves a light‑based intervention (such as a specific lighting protocol in a lab), a personal light logger worn near the eyes allows researchers to confirm the exact light stimulus each participant actually experienced – accounting for head or eye movements that stationary room sensors might miss. Similarly, in clinical settings where light therapy or a prescribed daylight exposure is given, a wearable logger can confirm whether the participant truly received the required dose of light."),
  p("For short‑duration measurements, we recommend using a corneal‑plane light logger (worn near the eye) to capture light exposure with high precision. Since the device is worn only briefly in a controlled environment, integration into daily life and long‑term comfort are less of a concern in this scenario."),
  
  h4("Medium‑Term Use (≤ 21 days): Characterising Habitual Exposure"),
  p("For medium‑term deployments (on the order of weeks), the goal is often to record a ‘slice’ of a person’s typical daily light exposure and relate it to short‑term outcomes. Researchers can use these multi‑week measurements to correlate light exposure with health metrics measured during the same period – for example, melatonin levels, cognitive performance, or sleep–wake patterns. Additionally, some laboratory protocols (such as maintaining a regular schedule for circadian stabilisation) ask participants to wear light loggers to ensure their ambient light exposure stays within expected ranges during the protocol."),
  p("In a medium‑term study, the logger will be part of nearly every aspect of the participant’s daily life – at home, at work or school, during leisure activities, and in public spaces. Therefore, the device must be comfortable and discreet enough for continuous wear, and socially acceptable so it does not disrupt the user’s routine or draw unwanted attention. Form factor and wearing location become important, as the logger should blend into daily life while still reliably recording data."),
  p("Depending on the research question, wearing location can also influence data quality. For example, if the focus is on evening light exposure, where even small changes in ambient light can have a strong physiological impact, it is important to measure light close to the eyes. In contrast, when assessing daytime light exposure (often in the range of 10,000 lux or more)  minor variations in wearing location are less critical, as high ambient light levels tend to saturate most sensors and differences in body placement are likely minimal."),
  
  h4("Long‑Term Use (> 21 days): Characterising Longitudinal Exposure"),
  p("For long‑term studies (many weeks to months or longer), researchers are interested in linking light exposure patterns with long‑term health outcomes. In these cases, a measurement period of just a few days or weeks may not adequately represent an individual’s true long‑term light exposure – for example, a two‑week recording in winter could be vastly different from one in summer. Truly long‑term monitoring is often needed to capture seasonal and other gradual variations in light exposure. To date, very few studies have successfully tracked personal light exposure continuously over months or years alongside health outcomes, making this an emerging area of research."),
  p("For long‑term deployments, ease of use and participant acceptance are paramount. The device must integrate seamlessly into the person’s life for extended periods. If the logger is cumbersome or intrusive, participants are likely to stop using it, risking attrition in the study. Minimising burden on the user – by making the logger as comfortable, low‑maintenance, and unobtrusive as possible – is key to keeping compliance high over long durations."),
  
  h3("Usability and Fidelity Trade‑Off"),
  p("It is important to recognise that there is a fundamental trade‑off between long‑term wearability and measurement fidelity. The most accurate light measurements usually come from devices worn near the eye or using more complex sensors, but those configurations tend to be less comfortable or convenient for extended use. Unless such devices can be greatly miniaturised or embedded into everyday items (for example, integrated into regular eyewear), researchers must balance obtaining higher fidelity data with ensuring the device is tolerable for participants. As of 2025, no available light logger perfectly achieves both high fidelity and seamless long‑term usability – in practice, collecting extremely accurate, eye‑level light data over very long periods is not yet feasible without compromising on user comfort or compliance.")
)

how_to <- tagList(
  p("The purpose of this tool is to help researchers to collect specifications for wearable devices. This can be useful in two ways. First, by providing a structured approach to collect and compare specifications, and second by providing an expert-informed list of (possibly) relevant specifications for a device. Down below, the section ", strong("How to select a wearable light logger or optical radiation dosimeter?"), " lays out a few general thoughts researchers should keep in mind. The page ", strong("Specification"), " allows the specification of relevant parameters of a light logger alongside several dimensions. These specifications can be exported into a PDF, or a MS Word file, to be used during the procurement phase for wearable devices. The specification can be sent to manufacturers, which can add their actual device specifications alongside the requirements. This way, researchers have the necessary information in one place and can weigh up- and downsides of a given device in a comprehensive list."),
  tags$
    div(
      class = "alert alert-warning",  # try alert-success / -warning / -danger too
      tags$h4(class = "alert-heading", "What if I can't provide all specifications?"),
      p("Don’t worry - this list is intentionally comprehensive. You won’t need to complete every field, and none are mandatory. If you don’t have a requirement for a question, simply leave it blank. If the exact option you need isn’t available and there’s no 'Other' field, you can edit the exported Word document to match your requirements. The goal is to make leaving a specification blank a deliberate choice, not an oversight."),
    ),
  strong("The url of this site supports bookmarking. You will be able to create a bookmark-url to restore your current entries. Please be aware that some browsers have a fairly limited number of url-characters. You will only be able to restore a portion of your state if the url gets capped and will receive no error message. Use another browser if this is the case (e.g., Chrome). The link will also be stored in the output documents for convenient access."),
  h3(actionButton("to_specification_form",
                  span(strong("Start the specification")),
                  icon = icon("file-pen"), class = "btn-success btn-lg")),
  p("For more information on relevant considerations for wearable devices, please visit the ", a("RDA Research Guide for wearable light loggers and optical radiation dosimeters", href = "https://rda-wg-visualexperiencedata.github.io/ResearcherGuide/", .noWS = "after"), ", which provides many more considerations relating to device specifications."),
  p("If you used this application, please cite as:"),
  fluidRow(
    column(
      width = 8,  # 6/12 = 50%
      div(
        tags$pre(id = "apaText2", class = "citation-pre", width = "50%",
                 "Zauner, J., Stefani, O., Biller, A. M., Guidolin, C., & Spitschan, M. (2025). 
Wearable light logger and optical radiation dosimeter specification tool (Version 1.0.0) [Computer software]. 
Zenodo. https://doi.org/10.5281/zenodo.17487054
Available at https://tscnlab-wearable-devices-specification.share.connect.posit.cloud"
        ),
        actionButton("copy", "Copy", onclick = "copyAPA()")
      )    )
  ),
)

site_introduction <- 
  nav_panel(
    "Introduction", 
    verbatimTextOutput("query"),
    card(
      card_title("How to use this tool?", container = h2),
      card_body(how_to)
    ),
    card(
      card_title("How to select a wearable light logger?", container = h2),
      card_body(introPage)
    )
  )