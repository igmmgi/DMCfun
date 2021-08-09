ui <- shiny::fluidPage(
  shiny::titlePanel("DMC Simulation"),
  shiny::mainPanel()
)

shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 2, style = "margin: 20px; overflow-y:scroll; max-height: 100%; font-size:12px",

        shiny::sliderInput(inputId = "amp",      label = "amp",      min =     0,    max =  50,     value =     20,   ticks = FALSE, step = 1,    width = 250),
        shiny::sliderInput(inputId = "tau",      label = "tau",      min =     1,    max = 200,     value =     30,   ticks = FALSE, step = 1,    width = 250),
        shiny::sliderInput(inputId = "drc",      label = "drc",      min =     0.1,  max =   1,     value =      0.5, ticks = FALSE, step = 0.01, width = 250),
        shiny::sliderInput(inputId = "bnds",     label = "bnds",     min =     20,   max = 150,     value =     75,   ticks = FALSE, step = 5,    width = 250),
        shiny::sliderInput(inputId = "resMean",  label = "resMean",  min =     100,  max = 500,     value =     300,  ticks = FALSE, step = 5,    width = 250),
        shiny::sliderInput(inputId = "resSD",    label = "resSD",    min =     0,    max = 100,     value =     30,   ticks = FALSE, step = 2,    width = 250),
        shiny::sliderInput(inputId = "aaShape",  label = "aaShape",  min =     1,    max =   5,     value =      2,   ticks = FALSE, step = 0.2,  width = 250),
        shiny::sliderInput(inputId = "spShape",  label = "spShape",  min =     1,    max =   5,     value =      3,   ticks = FALSE, step = 0.2,  width = 250),
        shiny::sliderInput(inputId = "sigm",     label = "sigm",     min =     1,    max =   6,     value =      4,   ticks = FALSE, step = 0.2,  width = 250),
        shiny::sliderInput(inputId = "nTrl",     label = "nTrl",     min =  5000,    max = 100000,  value =  20000,   ticks = FALSE, step = 1000, width = 250),
        shiny::sliderInput(inputId = "nTrlData", label = "nTrlData", min =     1,    max = 100,     value =      5,   ticks = FALSE, step =    1, width = 250),

        shiny::radioButtons("plottype", "Plot Type", choiceNames = c("Summary1", "Summary2", "Summary3", "RT Distribution"), choiceValues = c(1, 2, 3, 4)),
        shiny::sliderInput(inputId = "spDist", label = "Starting Point Distribution", min = 0, max = 2, value = 0, ticks = FALSE, step = 1, width = 250),
        shiny::sliderInput(inputId = "drDist", label = "Drift Rate Distribution", min = 0, max = 2, value = 0, ticks = FALSE, step = 1, width = 250),
        shiny::checkboxInput("autoaxis", "Auto Axis", FALSE),  shiny::verbatimTextOutput("autoaxis")

      ),

      shiny::mainPanel(width = 10, position = "right", fluid = FALSE, style = "margin-top:10px",
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
        resMean = input$resMean, resSD = input$resSD, aaShape = input$aaShape, spShape = input$spShape,
        sigm = input$sigm, nTrl = input$nTrl, fullData = fullData, nTrlData = input$nTrlData, spDist = input$spDist,
        drDist = input$drDist, printInputArgs = FALSE, printResults = FALSE)
      if (input$plottype == 1) {
        plot(dmc, figType = "summary1", cex = 2, cex.lab = 2.5, cex.axis = 2.5, mar = c(6,6,2,2), legend = FALSE, lwd = 3,
          xlimDelta = xlimDelta, ylimDelta = ylimDelta)
        legend(0.15, 0.75, legend = c("Compatible", "Incompatible"), lty = c(1,1), col = c("green", "red"), cex = 1.5)
      } else if (input$plottype == 2) {
        plot(dmc, figType = "summary2", cex = 2, cex.lab = 2.5, cex.axis = 2.5, mar = c(6,6,2,2), legend = FALSE, lwd = 3,
          xlimDelta = xlimDelta, ylimDelta = ylimDelta)
        legend(0.7, 0.5, legend = c("Compatible", "Incompatible"), lty = c(1,1), col = c("green", "red"), cex = 1.5)
      } else if (input$plottype == 3) {
        plot(dmc, figType = "summary3", cex = 1.5, mar = c(2,6,2,2), lwd = 3,
          ylimRt = ylimRt, ylimErr = ylimErr)
      } else if (input$plottype == 4) {

        # histogram of RT distributions
        par(mfrow = (c(2, 1)))
        par(mar = c(0, 4, 2, 2))

        # Correct RTs
        comp   <- dmc$sim$rts_comp
        y      <- length(comp) / 10
        hist(comp,
          xlim = c(0, 1000), ylim = c(0, y),
          xaxt = "n", col = scales::alpha('green', .5), border = FALSE,
          breaks = 100, main = "", yaxt = "n", xlab = "", ylab = "")
        abline(v = mean(comp), col = "green", lwd = 2)
        legend("topright", c("Compatible", "Incompatible"), fill = c("green", "red"),
          bty = "n", cex = 2)

        incomp <- dmc$sim$rts_incomp
        hist(incomp,  add = TRUE,
          xlim = c(0, 1000), ylim = c(0, y),
          xaxt = "n", col = scales::alpha('red', .5),
          border = FALSE, breaks = 100, main = "", xlab = "", ylab = "")
        abline(v = mean(incomp), col = "red", lwd = 2)

        # Error RTs
        par(mar = c(5, 4, 0, 2))
        comp   <- dmc$sim$errs_comp

        if (length(comp) > 0) {
          hist(comp,
            xlim = c(0, 1000), ylim = c(y, 0),
            col = scales::alpha('green', .5), border = FALSE,
            breaks = 100, main = "", yaxt = "n", xlab = "Time [ms]", ylab = "", cex.axis = 1.5, cex.lab = 2)
          abline(v = mean(comp), col = "green", lwd = 2)
        }

        incomp <- dmc$sim$errs_incomp
        if (length(incomp) > 0) {
          hist(incomp, add = TRUE,
            xlim = c(0, 1000), ylim = c(y, 0),
            col = scales::alpha('red', .5),
            border = FALSE, breaks = 100, main = "", ylab = "")
          abline(v = mean(incomp), col = "red", lwd = 2)
        }

      }
    })
  }
)
