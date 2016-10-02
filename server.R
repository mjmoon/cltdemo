#############################################################
# cltdemo - visualize central limit theorem by michael moon #
#                                                           #
# Shiny server                                              #
# rsconnect::deployApp(".") to deploy                       #
#############################################################
shinyServer(function(input, output, session) {
  source("plots.R")
  dt <- gendt(100, 500)
  output$sampPlot <- renderPlot(
    plotdt(dt, c(0, 1), 100)
    )
  output$qqPlot <- renderPlot(
    plotqq(dt, c(0, 1))
    )
  output$distPlot <- renderPlot(isolate(plotdist(input$dist, c(input$parm1, input$parm2))))
  
  observeEvent(input$dist, {
    if(input$dist == 1) {
      updateNumericInput(session, "parm2", "Standard Deviation", 1, 0.1, 2, 0.1)
      updateNumericInput(session, "parm1", "Mean", 0, -1.5, 1.5, 0.1)
      output$distPlot <- renderPlot(plotdist(1, c(0, 1)))
    } else if(input$dist == 2) {
      updateNumericInput(session, "parm1", "Alpha", 1, 0.5, 5, 0.5)
      updateNumericInput(session, "parm2", "Beta", 1, 0.5, 5, 0.5)
      output$distPlot <- renderPlot(plotdist(2, c(1, 1)))
    } else if(input$dist ==3) {
      updateNumericInput(session, "parm2", "Probability", 0.5, 0.05, 0.95, 0.05)
      updateNumericInput(session, "parm1", "Number of Trials", 1, 1, 50, 1)
      output$distPlot <- renderPlot(plotdist(3, c(1, 0.5)))
    }
  })
  
  observeEvent(input$parm1,
               output$distPlot <- renderPlot(isolate(plotdist(input$dist, c(input$parm1, input$parm2))))
               )
  observeEvent(input$parm2,
               output$distPlot <- renderPlot(isolate(plotdist(input$dist, c(input$parm1, input$parm2))))
               )
  
  observeEvent(input$sim, {
    dt <- isolate(gendt(input$nsamp, input$niter, input$parm1, input$parm2, input$dist))
    parms <- c(input$parm1, input$parm2)
    output$sampPlot <- renderPlot(isolate(plotdt(dt, parms, input$nsamp, input$dist)))
    output$qqPlot <- renderPlot(isolate(plotqq(dt, parms, input$dist)))
    }
    )
})
