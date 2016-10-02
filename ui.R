#############################################################
# cltdemo - visualize central limit theorem by michael moon #
#                                                           #
# Shiny user interface                                      #
#############################################################
shinyUI(fluidPage(
  
  titlePanel("Central Limit Theorem"),
  # Sidebar with a inputs 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("dist", "Distribution", 
                     list("Normal" = 1, 
                          "Beta" = 2,
                          "Binomial" = 3)),
      br(),
      numericInput("parm1", "Mean", 0, -1.5, 1.5, 0.1, "100%"),
      br(),
      numericInput("parm2", "Standard Deviation", 1, 0.1, 2, 0.1, "100%"),
      br(),
      sliderInput("nsamp", "Sample Size per Simulation", 10, 500, 100, 10),
      br(),
      sliderInput("niter", "Number of Simulations", 50, 2000, 500, 50),
      br(),
      actionButton("sim", strong("Simulate"), 
                   width = "100%", height = "150px",
                   style = "background-color:#2377BA; color:white")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      column(12, h5("Sample Means and Density"),
             plotOutput("sampPlot", height = "350px")
             ),
      column(6, h5("Population Density"),
             plotOutput("distPlot", height = "200px")
             ),
      column(6, h5("Q-Q Plot"),
             plotOutput("qqPlot", height = "200px")
             )
    ),
    position = "right"
  )
))
