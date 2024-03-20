library(dplyr)
library(shiny)
library(shinysurveys)
library(tidyr)

##################################
# Assemble question answers (some may be very long)
df <- data.frame(question = "How many grams weighed?",
                 option = NA,
                 input_type = "numeric",
                 input_id = "harvest_weight",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)

many_options <- data.frame(question = "What kind of input?",
                 option = c("document_type1", "document_type2"),
                 # Use the c() vector of character options to pull in a unique set of values here as long as needed
                 input_type = "select",
                 input_id = "input_type",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)

date_question <- data.frame(
  question = "What date did you observe this?",
  option = NA,
  input_type = "date",
  input_id = "obs_date",
  dependence = NA,
  dependence_value = NA,
  required = FALSE
)


df <- rbind(df,many_options, date_question)

# Add input types a la https://shinysurveys.jdtrat.com/articles/custom-input-extensions.html
# Register a date input to {shinysurveys}, limiting possible dates to a twenty-day period.
extendInputType("date", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-10,
    max = Sys.Date()+10
  )
})


# Create the User Interface

ui <- fluidPage(
  # Add an input for observer.  This will become subject_id.
  # This function from shinysurveys package generates the survey questions.
  surveyOutput(df = df,
               survey_title = "Generic Data Input",
               survey_description = "This is Claire's test at data input into Google Sheets."),
  
  # This standard shiny function generates a plot that only shows up after being submitted
  # due to the observeEvent in the server function below.
  plotOutput('plot')


  
  )

# Activities on the server

server <- function(input, output, session) {

# Function from package shinysurveys to render the survey.
  renderSurvey()

# Click submit.
  observeEvent(input$submit, {

    #at submit, get survey data into response_data frame    
    response_data <- getSurveyData(  custom_id = "test",
                                     include_dependencies = TRUE, # from help: "The number of rows, corresponding to the questions an individual saw, depends on the include_dependencies argument. If TRUE, by default, then the resulting data frame will have one row per unique input ID. "
                                     dependency_string = "HIDDEN-QUESTION"
                                     )
    
    # display data for confirmation it worked
    print(response_data)
    
    # Then wrangle into format for google sheet rows

    response_wide <- response_data %>%
      dplyr::select(-question_type) %>%
      pivot_wider(id_cols = subject_id,
                names_from = question_id,
                values_from = response) %>%
      dplyr::mutate(harvest_weight =  as.numeric(harvest_weight),
                    obs_date = as.Date(obs_date)) 

    
    print(response_wide)
    
    # Then submit to google sheets with reactive
    # https://www.jdtrat.com/blog/connect-shiny-google/
    # Other options:
    #   https://medium.com/@joyplumeri/using-r-shiny-to-create-web-surveys-display-instant-feedback-and-store-data-on-google-drive-68f46eea0f8b
    #   https://medium.com/@predict42/google-sheets-in-r-shiny-a-happy-marriage-8812303711ac

    # Alternatively, do a file upload
    # https://googledrive.tidyverse.org/ or osfr package
    
    
    showModal(modalDialog(title = "You have entered an observation.",
                          "Look below to see how these data compare to others."))
    
    # plot numeric data
    output$plot <- renderPlot({
      #right now this plots the current data point.
           plot(y = response_wide$harvest_weight,
                x = response_wide$obs_date)
      # make it so it pulls from the full data frame and plots the new answer on existing data for reference.
      })
  })
}


# Run the app.  
shinyApp(ui, server)
