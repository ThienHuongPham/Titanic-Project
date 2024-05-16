library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

# Reading dataset
data <- read.csv("titanic_data.csv") 

#Take a glimpse
glimpse(data)

# checking column names of the dataset and the number of missing values in each column
colSums(is.na(data))

#Print values in some columns and the number of corresponding value
data %>%
  count(Embarked) %>%
  print()

data %>%
  count(Sex) %>%
  print()

data %>%
  count(Pclass) %>%
  print()

data %>%
  count(Survived) %>%
  print()

#Change data type of Age column into integer
data$Age <- as.integer(data$Age)

#Create a sub-dataset by converting the specified columns to factor variables 
#and dealing with 2 blank data in Embarked column and 177 missing value in Age column
sub_data <- data %>%
  mutate(across(c(Survived, Pclass, Sex, Embarked), as.factor)) %>%
  filter(Embarked %in% c("S", "C", "Q")) %>%
  filter(!is.na(Age))


ui <- dashboardPage(
  dashboardHeader(title = "Titanic 1912 Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    includeCSS("styles.css"),
    fluidRow(
      tabBox(
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", width = 12, 
        tabPanel(strong("Overview"), 
                 hr(),
                 fluidRow(
                   box(
                     checkboxGroupInput("pclass_selector", "Choose PClass:", inline = T,
                                        choices = list("1", "2", "3"),
                                        selected = c("1","2","3")),
                     
                     checkboxGroupInput("sex_selector", "Choose Sex:", inline = T,
                                        choices = list("female", "male"),
                                        selected = c("female","male")),
                     
                     checkboxGroupInput("emb_selector", "Choose port of embarkation:",inline = T,
                                        choices = list("Q","S","C"),
                                        selected = c("Q","S","C")),
                     
                     sliderInput("age_slider", "Filter by Age", min = 0, max = 100, value = c(0, 100)),
                     
                     sliderInput("fare_slider", "Filter by ticket fare", min = 0, max = 520, value = c(0, 520)),
                     
                     radioButtons("parch_selector", "Parents/children onboard",inline = T,
                                  choices = c("Yes", "No", "Regardless"),
                                  selected = "Regardless"),
                     
                     radioButtons("sibsp_selector", "Siblings/spouses onboard",inline = T,
                                  choices = c("Yes", "No", "Regardless"),
                                  selected = "Regardless"),
                     width = 3
                   ),
                   valueBox(nrow(sub_data), "Total passengers", icon("user"), color="light-blue", width=3), 
                   valueBox(paste(round(sum(sub_data$Survived == 1)/nrow(sub_data)*100,1),"%"),
                            "Survival rate",
                            icon = icon("heart", lib = "font-awesome"),
                            color="light-blue",width=2),
                   valueBox(paste(round(sum(sub_data$Sex == "female")/nrow(sub_data)*100,1),"%"),
                            "Female rate",
                            icon = icon("venus", lib = "font-awesome"),
                            color="light-blue",width=2),
                   valueBox(round(mean(sub_data$Age),1), "Average age",
                            icon = icon("hourglass", lib = "font-awesome"),
                            color="light-blue",width=2),
                   box(plotOutput("survivalPieChart", width=450)),
                   infoBoxOutput("sampleSize", width=3), 
                   infoBoxOutput("survival", width=3),
                   infoBoxOutput("notSurvival", width=3),
                   infoBoxOutput("avgAge", width=3)
                 ),
                 
                 fluidRow(
                   box(
                     DTOutput("myTable"), width=12
                   )
                 ),
                 class = "tab1-background"

             ),
        tabPanel(strong("More insights"),
                 fluidRow(
                   hr(),
                   box(
                     width = 2,
                     radioButtons("criteria", "Choose criterion", 
                                  c( Age = "Age", Sex = "Sex", Pclass = "Pclass", SibSp = "SibSp", Parch = "Parch",
                                    Embarked = "Embarked", Fare="Fare")),
                   ),
                   box(
                     width = 4,
                     plotOutput("survivalPlot")
                   ),
                   box(
                     width = 2,
                     selectInput("plotType", "Choose by mixed criteria", choices = c("PClass and Gender", "Age and Class")),
                     conditionalPanel(
                       condition = "input.plotType == 'PClass and Gender'",
                       radioButtons("sex", "Choose  ", choices = c("All","Survivors", "Non-Survivors"))
                     )
                   ),
                   box(
                     width = 4,
                     plotOutput("mixedPlot")
                   ),
                   class = "tab1-background"
                 )
                 
          )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    sub_data %>%
      filter(Pclass %in% input$pclass_selector,
             Sex %in% input$sex_selector,
             Embarked %in% input$emb_selector,
             Age >= input$age_slider[1] & Age <= input$age_slider[2],
             Fare >= input$fare_slider[1] & Fare <= input$fare_slider[2],
             if (input$parch_selector == "Yes") Parch > 0
             else if (input$parch_selector == "No") Parch == 0
             else TRUE,
             if (input$sibsp_selector == "Yes") SibSp > 0
             else if (input$sibsp_selector == "No") SibSp == 0
             else TRUE
      )
  })
  
  
  output$myTable <- renderDT(
    filtered_data(),
    options = list(
      orderClasses = TRUE,
      pageLength = 10
    )
  )
  
  output$survivalPieChart <- renderPlot({
    survived_counts <- sum(filtered_data()$Survived == 1)
    died_counts <- sum(filtered_data()$Survived == 0)
    
    survival_rate <- sum(filtered_data()$Survived == 1) / nrow(filtered_data()) * 100
    died_rate <- sum(filtered_data()$Survived ==0) / nrow(filtered_data()) * 100
    
    pie(c(survived_counts, died_counts),
        labels = c(paste("Survived (", round(survival_rate, 1), "%)"), 
                   paste("Died (", round(died_rate, 1), "%)")),
        col = c( "#009E73","grey"),
        border = NA,
        main = "Survival rate based on the choosen criteria")
  })
  
  output$sampleSize <- renderInfoBox({
    infoBox("Sample size",nrow(filtered_data()), color="black")
  })
  
  output$survival <- renderInfoBox({
    infoBox("Survival",sum(filtered_data()$Survived == 1), icon = icon("heart", lib = "font-awesome"),
            color="olive")
  })
  
  output$notSurvival <- renderInfoBox({
    infoBox("Not survival",sum(filtered_data()$Survived == 0), icon = icon("heart", lib = "font-awesome"),
            color="orange")
  })
  
  output$avgAge <- renderInfoBox({
    infoBox("Average age",round(mean(filtered_data()$Age),1), icon = icon("hourglass"),
            color="aqua")
  })
  
  criteria_reactive <- reactive({
    input$criteria
  })
  
  output$survivalPlot <- renderPlot({
    inputValue <- criteria_reactive()
    sub_data$Survived <- ifelse(sub_data$Survived == 1, "Survived","Died")
    
    if (inputValue == "Sex") {
      table <- (table(sub_data$Sex, sub_data$Survived))
      mosaicplot(table, col = c("#da5f02", "#009E73"), main = "Death and Survival Rate by Gender", las = 1)
    } 
    else if (inputValue == "Pclass") {
      ggplot(sub_data, aes(x = Pclass, fill = Survived)) +
        geom_bar(position = "dodge", color = "black", stat = "count")+
        labs(title = "Survival Rate by PClass", x = "Passenger Class", y = "Number of Passengers") +
        theme_gray()+scale_fill_manual(values = c("#da5f02", "#009E73"))
    } 
    else if (inputValue == "Age") {
      ggplot(sub_data, aes(x = Age, fill = Survived)) + 
        geom_histogram(binwidth = 5, color = "white") + 
        geom_vline(aes(xintercept = mean(Age)), color = "darkblue") + 
        labs(title = "Survival Rate by Age", x = "Age", y = "Number of Passengers") + 
        theme_gray()+scale_fill_manual(values = c("#da5f02", "#009E73")) 
    }
    else if (inputValue == "SibSp") {
      ggplot(sub_data, aes(x = factor(SibSp), fill = Survived)) +
        geom_bar(position = "dodge", color = "black", stat = "count") +
        labs(title = "Survival rate by Sibsp", x = "Number of siblings/spouses", y = "Number of Passengers") +
        theme_gray()+scale_fill_manual(values = c("#da5f02", "#009E73"))
    }
    else if (inputValue == "Parch") {
      ggplot(sub_data, aes(x = factor(Parch), fill = Survived)) +
        geom_bar(position = "fill", color = "black") +
        labs(title = "Survival Rate by Parch", x = "Number of parents/children", y = "Survival Rate") +
        theme_gray()+scale_fill_manual(values = c("#da5f02", "#009E73"))
    }
    else if (inputValue == "Embarked") {
      ggplot(sub_data, aes(x = Embarked, fill = Survived)) +
        geom_bar(position = "stack", color = "black", stat = "count") +
        labs(title = "Survival rate by Port of Embarktion", x = " Port of embarkation", y = "Number of Passengers") +
        theme_gray()+scale_fill_manual(values = c("#da5f02", "#009E73"))
    }
    else if (inputValue == "Fare") {
      ggplot(sub_data, aes(x = Survived, y = Fare, fill = Survived)) +
        geom_boxplot() +
        labs(title = "Survival Rate by Price of Ticket", x = "Survival Status", y = "Price of Ticket") +
        theme_gray()+scale_fill_manual(values = c("#da5f02", "#009E73"))
    }
  })
  
  output$mixedPlot <- renderPlot({
    plotType <- input$plotType
    
    if (plotType == "Age and Class") {
      sub_data$Survived <- ifelse(sub_data$Survived == 1, "Survived","Died")
      ggplot(sub_data, aes(x = Age, fill = Survived)) +
        geom_histogram(binwidth = 5, color = "white", position = "stack") +
        facet_grid(Sex ~ Pclass, scales = "free_y") +
        theme_gray()+ labs(title = "Survival and Death Distribution by Age and Class",
                         x = "Age", y = "Number of Passengers", fill = "Class") +
        theme(legend.position = "bottom")+scale_fill_manual(values = c("#da5f02", "#009E73"))
    } 
    else if (plotType == "PClass and Gender") {
      sex_reactive <- reactive({input$sex})
      inputValue <- sex_reactive()
      survivors <- sub_data[sub_data$Survived == 1, ]
      death <- sub_data[sub_data$Survived == 0,]
      
      if (inputValue == "All") {
        ggplot(sub_data, aes(x = factor(Pclass), y = Age, fill = Sex)) +
          geom_boxplot() + labs(title = "Age distribution of passengers by PClass and Gender",
                                x = "Passenger Class", y = "Age", fill = "Gender") +
          theme_gray() + scale_fill_brewer(palette = "Dark2")
      }
      else if (inputValue == "Survivors") {
        ggplot(survivors, aes(x = factor(Pclass), y = Age, fill = Sex)) +
          geom_boxplot() + labs(title = "Age distribution of Survivors by PClass and Gender",
                                x = "Passenger Class", y = "Age", fill = "Gender") +
          theme_gray() + scale_fill_brewer(palette = "Dark2")
      }
      else if (inputValue == "Non-Survivors") {
        ggplot(death, aes(x = factor(Pclass), y = Age, fill = Sex)) +
          geom_boxplot() + labs(title = "Age distribution of Non-Survivors by PClass & Gender",
                                x = "Passenger Class", y = "Age", fill = "Gender") +
          theme_gray() + scale_fill_brewer(palette = "Dark2")
      }
    }
  })
  
  
}

shinyApp(ui = ui, server = server)



