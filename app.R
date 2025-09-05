library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(scales)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

PASSCS_Grades <- read_excel("Grade_v2.xlsx", sheet = "PASSCS") %>%
  filter(!is.na(Grade)) %>%
  mutate(Term = case_when(
    Term == "2025 Spring D Term" ~ "D",
    Term == "2025 Spring C Term" ~ "C",
    Term == "2024 Fall B Term" ~ "B",
    Term == "2024 Fall A Term" ~ "A",
    Term == "2025 Spring Semester" ~ "C/D",
    Term == "2024 Fall Semester" ~ "A/B"
  )) %>%
  mutate(`Course Code` = sub("-.*", "", `Course Section`)) %>%
  select(-c("Course Section", "Pass CS Status", "Semester")) %>%
  mutate(Pell = "PASSCS") %>%
  distinct() %>%
  arrange(Student)

Not_PASSCS_Grades <- read_excel("Grade_v2.xlsx", sheet = "Not PASSCS") %>%
  filter(!is.na(Grade)) %>%
  mutate(Term = case_when(
    Term == "2025 Spring D Term" ~ "D",
    Term == "2025 Spring C Term" ~ "C",
    Term == "2024 Fall B Term" ~ "B",
    Term == "2024 Fall A Term" ~ "A",
    Term == "2025 Spring Semester" ~ "C/D",
    Term == "2024 Fall Semester" ~ "A/B"
  )) %>%
  mutate(`Course Code` = sub("-.*", "", `Course Section`)) %>%
  select(-c("Course Section", "Pass CS Status", "Semester")) %>%
  mutate(Pell = case_when(
    Pell == "Pell" ~ Pell,
    TRUE ~ "Not Pell"
  )) %>%
  distinct() %>%
  arrange(Student)

Grades_Full_cleaned <- bind_rows(PASSCS_Grades, Not_PASSCS_Grades)

COurseCredits <- read_excel("Grade_v2.xlsx", 
                            sheet = "CourseCredits")

Grades_Full_cleaned <- left_join(Grades_Full_cleaned, COurseCredits, by = c("Course Code" = "Course")) %>%
  mutate(GPA = case_when(
    Grade == "A" ~ 4,
    Grade == "B" ~ 3,
    Grade == "C" ~ 2,
    TRUE ~ NA_real_  
  ),
  Subject = str_extract(`Course Code`, "^[^ ]+")) %>%
  mutate(Grade = case_when(
    Grade == "NR" ~ "NR/I",
    Grade == "I" ~ "NR/I",
    TRUE ~ Grade
  ))

NR_by_class <- Grades_Full_cleaned %>%
  filter(Credits == 3) %>%
  mutate(NRed = !(Grade %in% c("A","B","C","P"))) %>%
  group_by(`Course Code`, Subject) %>%
  summarise(
    NRs = sum(NRed),
    Total = n(),
    NR_Rate = NRs / Total,
    .groups = "drop"
  )

ui <- dashboardPage(
  dashboardHeader(title = "FY Grades"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("NR Rate", tabName = "NR_Rate", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "NR_Rate",
              
              # Row 1: Filters
              fluidRow(
                box(
                  width = 12,
                  title = "Filters",
                  status = "info",
                  solidHeader = TRUE,
                  # Wrap columns inside a fluidRow
                  fluidRow(
                    column(
                      width = 4,
                      pickerInput(
                        inputId = "Subject_Filter",
                        label = "Filter by Subject:",
                        choices = unique(NR_by_class$Subject),
                        selected = NULL,              # starts with nothing selected
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,      # adds Select All / Deselect All buttons
                          `live-search` = TRUE        # optional: adds search within dropdown
                        )
                      )
                    ),
                    column(
                      width = 4,
                      pickerInput(
                        inputId = "Look_Filter",
                        label = "Look by:",
                        choices = c("NR Rate", "NR Count", "Students Taken"),
                        selected = "NR Rate"
                      )
                    ),
                    column(
                      width = 4,
                      sliderInput("students_slider",
                                  label = "Select Number of Students Taken Course", 
                                  min = 0, 
                                  max = 225, 
                                  value = c(30, 225),  
                                  step = 10)
                    )
                  )
                )
              ),
              
              # Row 2: Radial plot
              fluidRow(
                box(
                  width = 12,
                  title = "NR Rate",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("radialPlot")
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    NR_by_class %>%
      filter(
        Subject %in% input$Subject_Filter,
        Total >= input$students_slider[1],
        Total <= input$students_slider[2]
      )
  })
  
  
  output$radialPlot <- renderPlotly({
    df <- filtered_data() %>%
      filter(!is.na(NR_Rate), !is.na(`Course Code`)) %>%
      mutate(y_value = case_when(
        input$Look_Filter == "NR Rate"  ~ NR_Rate,
        input$Look_Filter == "NR Count" ~ NRs,
        TRUE                            ~ Total
      ))
    
    if (nrow(df) == 0) {
      return(plotly_empty())
    }
    
    
    df$`Course Code` <- factor(df$`Course Code`, levels = unique(df$`Course Code`))
    
    p <- ggplot(df, aes(x = `Course Code`, y = y_value, fill = Subject)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "NR Rates by Course",
        y = input$Look_Filter,  # label reflects the selected metric
        x = ""
      ) +
      scale_y_continuous(
        labels = if (input$Look_Filter == "NR Rate") {
          scales::percent_format()
        } else {
          scales::comma_format()
        }
      ) +
      theme(axis.text.x = element_text(size = 10, face = "bold"))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
}


shinyApp(ui, server)
