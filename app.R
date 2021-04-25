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
           verticalLayout(
               textOutput("linear_regression"),
               plotOutput("tree")
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    filedata <- read.csv("MERGED2018_19_PP.csv", header=TRUE)
    
    inState <- reactive({input$in_state_tuition_choice})
    outState <- reactive({input$out_of_state_tuition_choice})
    percWomen <- reactive({input$percentage_of_women_choice})
    gradRate <- reactive({input$graduation_rate_choice})
    partTime <- reactive({input$part_time_choice})
    whiteStudent <- reactive({input$white_student_choice})
    multiStudent <- reactive({input$multirace_choice})
    federal <- reactive({input$federal_loan_choice})
    
    output$linear_regression <- renderText({
        good <- 1
        bad <- 0
        
        feature_vector <- c("PCIP27")
        if(inState() == 1) {
            feature_vector <- append(feature_vector, "TUITIONFEE_IN")
        }
        if(outState() == 1) {
          feature_vector <- append(feature_vector, "TUITIONFEE_OUT")
        }
        if (length(feature_vector) == 1) {
            print("Turn on variable to yes")
            paste("OUTPUT", bad)
        }
        else{
            print(feature_vector)
            paste("OUTPUT", good)
        }
        #add the rest later
        
        #necessary_features <- filedata[, feature_vector]
        #for (value in feature_vector) {
        #   necessary_features$value <- as.numeric(necessary_features$value)
        #}
        
        #necessary_features <- na.omit(necessary_features)
        
        #linear_regression_model = lm.fit(PCIP27~.-PCIP27, data=necessary_features)
        
        #paste(summary(linear_regression_model))
    })

    #output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
    #    bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
    #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
