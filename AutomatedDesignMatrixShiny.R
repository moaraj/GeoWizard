# Define UI
Step_4B

ui <- fluidPage(mainPanel(div(
     id = "AutoGen",
     h3("Automatic Generation"),
     wellPanel("Design Matrix",
               radioButtons(inputId = "StudyDesign",
                            label = "Select Study Design",
                            choices = c("TwoGroups",  # WT and Mut or Moelcule and No Molecule
                                        "nGroup Comparison", # Vehicle and Many Drugs,
                                        "PairedSamples", # Each sample has a control and pert
                                        "nFactorialDesign"),
                            uiOutput("DesignOptions")
          )),
     
     wellPanel("Contrast Matrix")
)))


# Server logic
server <- function(input, output, session) {
     observeEvent(StudyDesign{
          
     })
}
# Complete app with UI and server components
shinyApp(ui, server)
