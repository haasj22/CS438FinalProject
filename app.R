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
    
    navbarPage("MATH Team Project",
        tabPanel(
            "Model Creation Page",
            # Application title
            titlePanel("Create your own models using our cleaned dataset"),
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
                    verbatimTextOutput("linear_regression"),
                    plotOutput("tree")
                    )
                )
            ),
        ),
        tabPanel(
            "Model Display Page",
            titlePanel("Test some values on our best model"),
            sidebarLayout(
                sidebarPanel(
                    sliderInput("graduation_rate_to_predict", 
                            "School's gratuation rate",
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
                            "Perentage of students who are white",
                            min=0.0, 
                            max=1.0, 
                            step=0.01, 
                            value=0.5)
                ),
                
                mainPanel(
                    verbatimTextOutput("best_tree"),
                    h4("Your predicted college's percentage of math majors according to our model"),
                    verbatimTextOutput("prediction")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
    
    data_for_best_model <- filedata[, c("C150_4", "TUITIONFEE_IN", 
                                        "TUITIONFEE_OUT", "PPTUG_EF", 
                                        "UGDS_WHITE", "PCIP27")]
    data_for_best_model <-data.frame(lapply(data_for_best_model, as.numeric))
    data_for_best_model <- na.omit(data_for_best_model)
    best_model <- lm(PCIP27~poly(C150_4, 2) + poly(TUITIONFEE_IN, 1) 
                     + poly(TUITIONFEE_OUT, 4) + poly(PPTUG_EF, 2) 
                     + poly(UGDS_WHITE, 3), data=data_for_best_model)
    
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

    output$best_tree <- renderPrint({
        summary(best_model)
    })
    output$prediction <- renderPrint({
        predict(best_model, data.frame(C150_4 = c(gradRateToPredict()), 
                                       TUITIONFEE_IN = c(inStateToPredict()),
                                       TUITIONFEE_OUT = c(outStateToPredict()),
                                       PPTUG_EF = c(partTimeToPredict()),
                                       UGDS_WHITE = c(whiteStudentToPredict())))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
