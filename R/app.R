ui <- shiny::fluidPage(
  shiny::titlePanel("DMC Simulation"),
  shiny::mainPanel()
)

shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 2, style = "margin: 20px; overflow-y:scroll; max-height: 100%; font-size:14px",
        shiny::sliderInput(inputId = "amp",      label = "amp",      min =    0,    max = 50,      value =     30,   ticks = FALSE, step = 1,    width = 250),
        shiny::sliderInput(inputId = "tau",      label = "tau",      min =    1,    max = 200,     value =     20,   ticks = FALSE, step = 1,    width = 250),
        shiny::sliderInput(inputId = "drc",      label = "drc",      min =    0.1,  max = 1,       value =      0.5, ticks = FALSE, step = 0.1,  width = 250),
        shiny::sliderInput(inputId = "bnds",     label = "bnds",     min =    20,   max = 150,     value =     75,   ticks = FALSE, step = 5,    width = 250),
        shiny::sliderInput(inputId = "resMean",  label = "resMean",  min =    100,  max = 500,     value =     300,  ticks = FALSE, step = 5,    width = 250),
        shiny::sliderInput(inputId = "resSD",    label = "resSD",    min =    0,    max = 100,     value =     30,   ticks = FALSE, step = 2,    width = 250),
        shiny::sliderInput(inputId = "aaShape",  label = "aaShape",  min =    1,    max = 10,      value =      2,   ticks = FALSE, step = 0.2,  width = 250),
        shiny::sliderInput(inputId = "spShape",  label = "spShape",  min =    1,    max = 10,      value =      3,   ticks = FALSE, step = 0.2,  width = 250),
        shiny::sliderInput(inputId = "sigm",     label = "sigm",     min =    1,    max = 10,      value =      4,   ticks = FALSE, step = 0.2,  width = 250),
        shiny::sliderInput(inputId = "nTrl",     label = "nTrl",     min = 1000,    max = 200000,  value =  20000,   ticks = FALSE, step = 1000, width = 250),
        shiny::sliderInput(inputId = "nTrlData", label = "nTrlData", min =    1,    max = 100,     value =      5,   ticks = FALSE, step =    1, width = 250),

        shiny::checkboxInput("summary1", "Summary1", TRUE),  shiny::verbatimTextOutput("summary1"),
        shiny::checkboxInput("summary2", "Summary2", FALSE), shiny::verbatimTextOutput("summary2"),
        shiny::checkboxInput("summary3", "Summary3", FALSE), shiny::verbatimTextOutput("summary3")
      ),
      shiny::mainPanel(width = 10, position = "right", fluid = FALSE, style = "margin-top:10px",
        shiny::plotOutput("dmcSim", width = "100%", height = "1000px")
      )
    )
  ),
  server = function(input, output) {
    output$dmcSim <- shiny::renderPlot({
      dmc <- dmcSim(amp = input$amp, tau = input$tau, drc = input$drc, bnds = input$bnds,
        resMean = input$resMean, resSD = input$resSD, aaShape = input$aaShape, spShape = input$spShape,
        sigm = input$sigm, nTrl = input$nTrl, fullData = input$summary1, nTrlData = input$nTrlData)
      if (input$summary1) {
        plot(dmc, figType = "summary1")
      } else if (input$summary2) {
        plot(dmc, figType = "summary2")
      } else if (input$summary3) {
        plot(dmc, figType = "summary3")
      }
    })
  }
)
