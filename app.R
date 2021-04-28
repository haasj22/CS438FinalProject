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
library(ggplot2)
library(tree)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Math Team Final Dashboard"),
    h2("Choose variables to predict the amount of math majors at a college"),

    sidebarLayout(
        sidebarPanel(
            radioGroupButtons("in_state_tuition_choice", label = h6("Students paying in-state Tuition:"), 
                         choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("out_of_state_tuition_choice", label = h6("Students paying out-of-State Tuition:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("percentage_of_women_choice", label = h6("Students that are women:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("graduation_rate_choice", label = h6("Students that graduate in 4 year institutions:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("part_time_choice", label = h6("Part time student workers:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("white_student_choice", label = h6("Students who are white:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("multirace_choice", label = h6("Students who are two or more races:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE),
            radioGroupButtons("federal_loan_choice", label = h6("Students who are receiving federal loans:"), 
                              choices = list("Yes" = 1, "No" = 2), selected = 2, direction="horizontal", individual=FALSE)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verticalLayout(
               verbatimTextOutput("linear_regression"),
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
    
    output$linear_regression <- renderPrint({
        good <- 1
        bad <- 0
        
        feature_vector <- c("PCIP27")
        if(inState() == 1) {
            feature_vector <- append(feature_vector, "TUITIONFEE_IN")
        }
        if(outState() == 1) {
          feature_vector <- append(feature_vector, "TUITIONFEE_OUT")
        }
        if(percWomen() == 1) {
            feature_vector <- append(feature_vector, "UGDS_WOMEN")
        }
        if(gradRate() == 1) {
            feature_vector <- append(feature_vector, "C150_4")
        }
        if(partTime() == 1) {
            feature_vector <- append(feature_vector, "PPTUG_EF")
        }
        if(whiteStudent() == 1) {
            feature_vector <- append(feature_vector, "UGDS_WHITE")
        }
        if(multiStudent() == 1) {
            feature_vector <- append(feature_vector, "UGDS_2MOR")
        }
        if(federal() == 1) {
            feature_vector <- append(feature_vector, "PCTFLOAN")
        }
        if (length(feature_vector) == 1) {
            paste("Change at least one variable to yes")
        }
        else{
            #gets the correct columns from the dataset
            necessary_features <- filedata[, feature_vector]
            #converts the dataset to numbers
            necessary_features <- data.frame(lapply(necessary_features, as.numeric))
            #removes the null values
            necessary_features <- na.omit(necessary_features)
            #applies linear regression to the dataset
            linear_regression_model = lm(PCIP27~.-PCIP27, data=necessary_features)
            #prints a summary of the data
            summary(linear_regression_model)
        }
    })
    
    output$tree <- renderPlot({
        
        good <- 1
        bad <- 0
        
        feature_vector <- c("PCIP27")
        if(inState() == 1) {
            feature_vector <- append(feature_vector, "TUITIONFEE_IN")
        }
        if(outState() == 1) {
            feature_vector <- append(feature_vector, "TUITIONFEE_OUT")
        }
        if(percWomen() == 1) {
            feature_vector <- append(feature_vector, "UGDS_WOMEN")
        }
        if(gradRate() == 1) {
            feature_vector <- append(feature_vector, "C150_4")
        }
        if(partTime() == 1) {
            feature_vector <- append(feature_vector, "PPTUG_EF")
        }
        if(whiteStudent() == 1) {
            feature_vector <- append(feature_vector, "UGDS_WHITE")
        }
        if(multiStudent() == 1) {
            feature_vector <- append(feature_vector, "UGDS_2MOR")
        }
        if(federal() == 1) {
            feature_vector <- append(feature_vector, "PCTFLOAN")
        }
        if (length(feature_vector) == 1) {
            paste("Change at least one variable to yes")
        }
        else{
            #gets the correct columns from the dataset
            necessary_features <- filedata[, feature_vector]
            #converts the dataset to numbers
            necessary_features <- data.frame(lapply(necessary_features, as.numeric))
            #removes the null values
            necessary_features <- na.omit(necessary_features)
            
            set.seed(10)
            train = sample(1:nrow(necessary_features), nrow(necessary_features)/2)
            tree.school = tree(PCIP27~C150_4+UGDS_WOMEN+TUITIONFEE_IN+TUITIONFEE_OUT,necessary_features, subset=train)
            
            summary(tree.school)
            
        }
    })
    
}
    
# Run the application 
shinyApp(ui = ui, server = server)
