ui <- shiny::fluidPage(
  shiny::titlePanel("DMC Simulation"),
  shiny::mainPanel()
)

shiny::shinyApp(
  options = list(height = 1080),
  ui = shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 2, style = "margin: 10px; overflow-y:scroll; max-height: 10%; font-size:14px",

        shiny::sliderInput(inputId = "amp",             label = "amp",             min =     0,    max =    100,  value =     20,   ticks = FALSE, step = 1,    width = 200),
        shiny::sliderInput(inputId = "tau",             label = "tau",             min =     1,    max =    200,  value =     30,   ticks = FALSE, step = 1,    width = 200),
        shiny::sliderInput(inputId = "drc",             label = "drc",             min =     0.1,  max =      1,  value =      0.5, ticks = FALSE, step = 0.01, width = 200),
        shiny::sliderInput(inputId = "bnds",            label = "bnds",            min =     20,   max =    350,  value =     75,   ticks = FALSE, step = 5,    width = 200),
        shiny::sliderInput(inputId = "resMean",         label = "resMean",         min =     100,  max =    500,  value =    300,   ticks = FALSE, step = 5,    width = 200),
        shiny::sliderInput(inputId = "resSD",           label = "resSD",           min =     0,    max =    100,  value =     30,   ticks = FALSE, step = 2,    width = 200),
        shiny::sliderInput(inputId = "aaShape",         label = "aaShape",         min =     1,    max =      5,  value =      2,   ticks = FALSE, step = 0.2,  width = 200),
        shiny::sliderInput(inputId = "spShape",         label = "spShape",         min =     1,    max =      5,  value =      3,   ticks = FALSE, step = 0.2,  width = 200),
        shiny::sliderInput(inputId = "sigm",            label = "sigm",            min =     1,    max =      6,  value =      4,   ticks = FALSE, step = 0.2,  width = 200),
        shiny::sliderInput(inputId = "drOnset"  ,       label = "drOnset",         min =     0,    max =    300,  value =      0,   ticks = FALSE, step = 1,    width = 200),
        shiny::sliderInput(inputId = "bndsRate" ,       label = "bndsRate" ,       min =     0,    max =      2,  value =      0,   ticks = FALSE, step = 0.01, width = 200),
        shiny::sliderInput(inputId = "bndsSaturation",  label = "bndsSaturation",  min =     0,    max =   1000,  value =      0,   ticks = FALSE, step =    1, width = 200),
        shiny::sliderInput(inputId = "nTrl",            label = "nTrl",            min =  5000,    max = 100000,  value =  20000,   ticks = FALSE, step = 1000, width = 200),
        shiny::sliderInput(inputId = "nTrlData",        label = "nTrlData",        min =     1,    max =    100,  value =      5,   ticks = FALSE, step =    1, width = 200),

        shiny::radioButtons("plottype", "Plot Type", choiceNames = c("Summary1", "Summary2", "Summary3", "RT Distribution"), choiceValues = c(1, 2, 3, 4)),
        shiny::sliderInput(inputId = "spDist", label = "Starting Point Distribution", min = 0, max = 2, value = 0, ticks = FALSE, step = 1, width = 250),
        shiny::sliderInput(inputId = "drDist", label = "Drift Rate Distribution", min = 0, max = 2, value = 0, ticks = FALSE, step = 1, width = 250),
        shiny::checkboxInput("autoaxis", "Auto Axis", FALSE),  shiny::verbatimTextOutput("autoaxis")

      ),
      shiny::mainPanel(width = 10, position = "center", fluid = TRUE, style = "margin-top:10px",
        shiny::plotOutput("dmcSim", width = "100%", height = "1000px")
      )
    )
  ),
  server = function(input, output) {
    output$dmcSim <- shiny::renderPlot({
      fullData <- ifelse(input$plottype == 1, TRUE, FALSE)
      xlimDelta <- NULL
      ylimDelta <- NULL
      ylimRt    <- NULL
      ylimErr   <- NULL
      if (!input$autoaxis) {
        xlimDelta <- c(0, 1000)
        ylimDelta <- c(-20, 100)
        ylimRt    <- c(350, 550)
        ylimErr   <- c(0, 30)
      }
      dmc <- dmcSim(amp = input$amp, tau = input$tau, drc = input$drc, bnds = input$bnds,
        resMean = input$resMean, resSD = input$resSD, aaShape = input$aaShape, spShape = input$spShape, spLim = c(-input$bnds, input$bnds),
        drOnset = input$drOnset, bndsRate = input$bndsRate, bndsSaturation = input$bndsSaturation,
        sigm = input$sigm, nTrl = input$nTrl, fullData = fullData, nTrlData = input$nTrlData, spDist = input$spDist,
        drDist = input$drDist, printInputArgs = FALSE, printResults = FALSE)
      if (input$plottype == 1) {
        plot(dmc, figType = "summary1", cex = 2, cex.lab = 2.0, cex.axis = 2.0, mar = c(5,5,2,2), legend = FALSE, lwd = 3,
          xlimDelta = xlimDelta, ylimDelta = ylimDelta, xylabPos = 3)
        legend(0.15, 0.75, legend = c("Compatible", "Incompatible"), lty = c(1,1), col = c("green", "red"), cex = 1.5)
      } else if (input$plottype == 2) {
        plot(dmc, figType = "summary2", cex = 2, cex.lab = 2.0, cex.axis = 2.0, mar = c(5,5,2,2), legend = FALSE, lwd = 3,
          xlimDelta = xlimDelta, ylimDelta = ylimDelta, xylabPos = 3)
        legend(0.7, 0.5, legend = c("Compatible", "Incompatible"), lty = c(1,1), col = c("green", "red"), cex = 1.5)
      } else if (input$plottype == 3) {
        plot(dmc, figType = "summary3", cex = 1.5, mar = c(2,5,2,2), lwd = 3,
          ylimRt = ylimRt, ylimErr = ylimErr, xylabPos = 2)
      } else if (input$plottype == 4) {
        DMCfun:::plot_distribution(dmc)
      }
    })
  }
)
