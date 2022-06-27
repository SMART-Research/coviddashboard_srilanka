library(shiny)
library(DT)
library(shinyWidgets)
library(tidyverse)


load("train.rda")
train$District <- factor(train$District)
train$Gender <- factor(train$Gender)
train$Education <- factor(train$Education)
train$Agronomic_crops_availability <- factor(train$Agronomic_crops_availability)


this_table <- train[0,] %>% select( District, Gender,                       
                                   Age, Education,  Income,
                                   Non_income_generating_members,
                                   Agronomic_crops_availability, 
                                   Agri_contributing_family_members,
                                   Agri_contributing_employees,
                                   Land_size)


#-----------------------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("App to Predict Probability of Willingness to Grow Aromatic Medicinal Plants"),
  sidebarPanel(
    selectInput("Gender","Gender",choices = c("","Male","Female")),
    selectInput("District","District",choices = c("","Badulla","Batticaloa", "Hambantota", "Kurunegala", "Matara", "Rathnapura")),
    numericInput("Age", "Age", 20, min = 1, max = 90),
    selectInput("Education","Education: PD-Passed Degree, PA-Passed AL, PO-Passed O/L, P10-Passed grade 10, P6-Passed grade 5, NS-No schooling",choices = c("","PD","PA", "PO", "P10", "P6", "NS")),
    numericInput("Income", "Income", 0, min = 0, max = 100000000),
    numericInput("Non_income_generating_members", "Number of non-income family generating members", 0, min = 0, max = 100000000),
    selectInput("Agronomic_crops_availability","Agronomics crops availability",choices = c("","Yes","No")),
    numericInput("Agri_contributing_family_members", "Number of agri-contributing family members", 0, min = 0, max = 100000000),
    numericInput("Agri_contributing_employees", "Number of agri-contributing employees", 0, min = 0, max = 100000000),
    numericInput("Land_size", "Land size", 0, min = 0, max = 100000000),
    
    actionButton("add_btn", "Add"),
    actionButton("delete_btn", "Delete"),
    actionButton("predict_btn", "Predict")
  ),
  
  mainPanel(
    DTOutput("shiny_table"),
    hr(),
    DTOutput("prediction_table")
  )
)

server <- function(input, output) {
  
  this_table <- reactiveVal(this_table)
  
  observeEvent(input$add_btn, {
    t = rbind(data.frame(District = input$District, 
                         Gender = input$Gender,                       
                         Age = input$Age,
                         Education = input$Education,  
                         Income = input$Income,
                         Non_income_generating_members = input$Non_income_generating_members,
                         Agronomic_crops_availability = input$Agronomic_crops_availability, 
                         Agri_contributing_family_members = input$Agri_contributing_family_members,
                         Agri_contributing_employees = input$Agri_contributing_employees,
                         Land_size = input$Land_size), this_table())
    this_table(t)
  })
  
  observeEvent(input$delete_btn, {
    t = this_table()
    print(nrow(t))
    if (!is.null(input$shiny_table_rows_selected)) {
      t <- t[-as.numeric(input$shiny_table_rows_selected),]
    }
    this_table(t)
  })
  
  
  
  output$shiny_table <- renderDT({
    datatable(this_table(), selection = 'multiple', options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))
  })
  
  
  predict_df <- eventReactive(input$predict_btn, {
    
    train <- train
    
    
    Model <-  glm(formula = Willingness~District + Gender + Age + Education + Income + Non_income_generating_members + Agronomic_crops_availability + Agri_contributing_family_members + Agri_contributing_employees + Land_size
                  , family = binomial(link = "logit"), data= train)
    
    new_data <- this_table()
    new_data$District <- factor(new_data$District)
    new_data$Gender <- factor(new_data$Gender)
    new_data$Education <- factor(new_data$Education)
    new_data$Agronomic_crops_availability <- factor(new_data$Agronomic_crops_availability)

    
    df <- data.frame( PREDICTED_Probability= round(predict(Model, newdata = new_data, type="response"), 2))
    df
  })
  
  
  output$prediction_table <- DT::renderDT({
    predict_df()
  })
  
  
}

shinyApp(ui = ui, server = server)