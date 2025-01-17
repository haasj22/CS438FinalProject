#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Project Code created by John Haas, Ashley Kim, Nick Ohara
# Data Completed on April 28, 2021
#
# Analysis data on 2018-2019 college data from the US. Board of Education

#uses libaries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(tree)

# Define UI for application
ui <- fluidPage( theme = shinytheme("united"),
    
    #creates a navbar
    navbarPage("MATH Team Project",
        
        #tab for model creation panel
        tabPanel(
            "Model Creation Page",
            # Application title
            titlePanel("Create your own models using our cleaned dataset"),
            
            # Sidebar with a slider input for number of bins 
            h4("Choose variables to predict the amount of math majors at a college"),
            sidebarLayout(
                sidebarPanel(
                    h4("Choose variables to predict the amount of math majors at a college"),
                    #creates buttons that provide Yes or No choices 
                    #as to whether each feature should be included
                    
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
                           
                # Shows plots of the generated distribution
                mainPanel(
                    verticalLayout(
                        #tells the program to plot the linear regression model
                        h4("Your Generated Linear Regression Model"),
                        verbatimTextOutput("linear_regression"),
                        
                        #tells the program to plot the binary tree
                        h4("Your Generated Binary Tree Model"),
                        plotOutput("tree")
                    )
                )
            )
        ),
        
        #tab for displaying predictions for the models
        tabPanel(
            "Model Display Page",
            
            #gets user input for prediction
            titlePanel("Test some values on our best model"),
            sidebarLayout(
                
                #creates slidebars that get appropriate input ranges for each feature
                sidebarPanel(
                    sliderInput("graduation_rate_to_predict", 
                        "School's graduation rate",
                        min=0.0, 
                        max=1.0, 
                        step=0.01, 
                        value=0.5),
                    sliderInput("in_state_tuition_to_predict", 
                        "School's cost for in state tution",
                        min=0, 
                        max=60000, 
                        step=100, 
                        value=20000),
                    sliderInput("out_state_tuition_to_predict", 
                        "School's cost for out of state tution",
                        min=0, 
                        max=60000, 
                        step=100, 
                        value=20000),
                    sliderInput("part_time_job_percent_to_predict",
                        "Percentage of school's students that work part time jobs",
                        min=0.0, 
                        max=1.0, 
                        step=0.01, 
                        value=0.5),
                    sliderInput("white_student_percentage_to_predict",
                        "Percentage of students who are white",
                        min=0.0, 
                        max=1.0, 
                        step=0.01, 
                        value=0.5)
                    ),
                               
                mainPanel(
                    #gets the summary of the linear regression tree
                    verbatimTextOutput("best_tree"),
                        
                    #explains the model
                    h4("Model Analysis"),
                    p("As shown in this model, we our best model explains 33% percent of the variance.
                    While this is not an amazing result the model does shed light on a couple interesting correlations.
                    First the positive coefficients for the out of state tuition fee means that the variable 
                    has a positive correlation with the percentage of math majors.
                    Perhaps this is because schools that cost more have higher funding for their programming or 
                    it could mean that these colleges just spend more money on advertizing."),
                    p("In contrast, colleges with a higher percentage of white students have a lower percentage of math majors.
                    We do not know how to explain this trend, but maybe white students are more likely to choose other majors."),
                    p("Next, it's relieving for students that like math to know that colleges with higher graduation rates
                    tend to have more math majors."),
                    p("Finally there is fluctuation for what an increase in part time workers does to the amount of math majors.
                    Initial increases in part time labor show a decrease in math majors, but continued increase shows a rebound in math major percentages."),
                    p("The last thing of note is that the attributes of the female percentage of students, 
                    the proportion of multirace students, and the ability of students to get loans showed
                    no significance when trying to predict the number of math majors."),
                    h4("Your predicted college's percentage of math majors according to our model."),
                        
                    #prints the output of the prediction
                    verbatimTextOutput("prediction")
                    )
                )
            )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #reads the csv
    filedata <- read.csv("MERGED2018_19_PP.csv", header=TRUE)
    
    #variables used to create models
    inState <- reactive({input$in_state_tuition_choice})
    outState <- reactive({input$out_of_state_tuition_choice})
    percWomen <- reactive({input$percentage_of_women_choice})
    gradRate <- reactive({input$graduation_rate_choice})
    partTime <- reactive({input$part_time_choice})
    whiteStudent <- reactive({input$white_student_choice})
    multiStudent <- reactive({input$multirace_choice})
    federal <- reactive({input$federal_loan_choice})
    
    #variables used to predict off our best model
    gradRateToPredict <- reactive({input$graduation_rate_to_predict})
    inStateToPredict <- reactive({input$in_state_tuition_to_predict})
    outStateToPredict <- reactive({input$out_state_tuition_to_predict})
    partTimeToPredict <- reactive({input$part_time_job_percent_to_predict})
    whiteStudentToPredict <- reactive({input$white_student_percentage_to_predict})
    
    #grabs the appropriate columns to create the best model
    data_for_best_model <- filedata[, c("C150_4", "TUITIONFEE_IN", 
                                        "TUITIONFEE_OUT", "PPTUG_EF", 
                                        "UGDS_WHITE", "PCIP27")]
    
    #makes the best model with the features we have
    data_for_best_model <-data.frame(lapply(data_for_best_model, as.numeric))
    data_for_best_model <- na.omit(data_for_best_model)
    best_model <- lm(PCIP27~poly(C150_4, 2) + poly(TUITIONFEE_IN, 1) 
                     + poly(TUITIONFEE_OUT, 4) + poly(PPTUG_EF, 2) 
                     + poly(UGDS_WHITE, 3), data=data_for_best_model)
    
    #function for printing the model
    output$linear_regression <- renderPrint({
        
        #makes a feature vector containing all wanted features
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
        #exits function if no variables are chosen
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
    
    #function that prints the information on the best model
    output$best_tree <- renderPrint({
        summary(best_model)
    })
    
    #function that prints the predicted value on our best model
    output$prediction <- renderPrint({
        predict(best_model, data.frame(C150_4 = c(gradRateToPredict()), 
                                       TUITIONFEE_IN = c(inStateToPredict()),
                                       TUITIONFEE_OUT = c(outStateToPredict()),
                                       PPTUG_EF = c(partTimeToPredict()),
                                       UGDS_WHITE = c(whiteStudentToPredict())))
    })
    
    #function that plots a tree with the desired features
    output$tree <- renderPlot({
        
        #makes a feature vector containing all wanted features
        #NOTE code is repeaded from previous function
        #This is bad coding standard but we could not figure out a way
        #to work with reactive variables otherwise
        #If we had more time I think we'd be able to fix this.
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
        #exits function if no variables are chosen
        if (length(feature_vector) == 1) {
        }
        else{
            #gets the correct columns from the dataset
            necessary_features <- filedata[, feature_vector]
            #converts the dataset to numbers
            necessary_features <- data.frame(lapply(necessary_features, as.numeric))
            #removes the null values
            necessary_features <- na.omit(necessary_features)
            
            #creates a tree from the desired data
            set.seed(10)
            train = sample(1:nrow(necessary_features), nrow(necessary_features)/2)
            tree.school = tree(PCIP27~.-PCIP27,necessary_features, subset=train)
            
            #plots the tree
            plot(tree.school)
            text(tree.school,pretty=0)
        }
    })
    
}
    
# Run the application 
shinyApp(ui = ui, server = server)
