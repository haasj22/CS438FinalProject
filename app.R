#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install if not installed already
#install.packages("shinyWidgets")

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Math Team Final Dashboard"),
    h2("Choose variables to predict the amount of math majors at a college"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioGroupButtons("in_state_tuition_choice", label = h6("In State Tuition:"), 
                         choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("out_of_state_tuition_choice", label = h6("Out of State Tuition:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("percentage_of_women_choice", label = h6("Percentage of women:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("graduation_rate_choice", label = h6("Graduation rate at 4 year insitutions:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("part_time_choice", label = h6("Percentage of students who are part time workers:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("white_student_choice", label = h6("Percentage of students who are white:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("multirace_choice", label = h6("Percentage of students who are two or more races:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("federal_loan_choice", label = h6("Percentage of students who are receiving federal loans:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
    #    bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
    #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
