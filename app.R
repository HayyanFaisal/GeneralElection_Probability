library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(broom)
library(caret)
library(janitor)
library(shinyWidgets)

# Load and clean data
elections <- read.csv("elections.csv", stringsAsFactors = FALSE) %>%
  janitor::clean_names()

df_clean <- elections %>%
  filter(!is.na(pop), !is.na(male), !is.na(female),
         !is.na(total), !is.na(no_of_polling_stations), !is.na(obtained_votes)) %>%
  mutate(
    Gender_Ratio = male / (female + 1),
    Winning_Margin = obtained_votes - lag(obtained_votes, default = 0)
  )

vote_model <- lm(obtained_votes ~ pop + total + no_of_polling_stations + Gender_Ratio, data = df_clean)

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
)
)

ui <- dashboardPage(
  dashboardHeader(title = "Pakistan Elections Analysis (2008–2024)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("table")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Descriptive Stats", tabName = "stats", icon = icon("calculator")),
      menuItem("Probability Distribution", tabName = "prob_ci", icon = icon("chart-area")),
      menuItem("Regression & Prediction", tabName = "regression", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    # DARK MODE CSS
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .right-side {
          background-color: #1e1e2f !important;
          color: #ffffff !important;
        }
        .box, .box-header {
          background-color: #2c2c3e !important;
          color: #ffffff !important;
        }
        .main-header .logo {
          background-color: #222d32 !important;
          color: #ffffff !important;
        }
        .main-header .navbar {
          background-color: #222d32 !important;
        }
        .sidebar {
          background-color: #222d32 !important;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #1a1a2e !important;
        }
        .tab-content {
          background-color: #1e1e2f !important;
        }
        .dataTables_wrapper {
          color: #ffffff !important;
        }
        .box-title {
          color: #ffffff !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Dataset Preview", width = 12,
                    DTOutput("data_preview"))
              )
      ),
      
      tabItem(tabName = "eda",
              fluidRow(
                box(width = 12, title = "Filters",
                    selectInput("year_filter", "Select Year:",
                                choices = c("All", sort(unique(elections$year_election))),
                                selected = "All")
                )
              ),
              fluidRow(
                box(width = 6, title = "Votes by Party", plotlyOutput("votes_party")),
                box(width = 6, title = "Votes by Year", plotlyOutput("votes_year"))
              ),
              fluidRow(
                box(width = 6, title = "Polling Stations by Province", plotlyOutput("polling_province")),
                box(width = 6, title = "Gender Distribution", plotlyOutput("gender_distribution"))
              ),
              fluidRow(
                box(width = 6, title = "Turnout % by Province", plotlyOutput("turnout_province")),
                box(width = 6, title = "Top 10 Parties by Votes", plotlyOutput("top_parties"))
              ),
              fluidRow(
                box(width = 12, title = "Votes vs Polling Stations", plotlyOutput("votes_polling"))
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("info-circle"), "Summary Statistics"),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  tabsetPanel(
                    tabPanel("Table", tableOutput("summary_table")),
                    tabPanel("Raw Output", verbatimTextOutput("summary_stats"))
                  )
                )
              ),
              fluidRow(
                box(width = 12, title = "Party-wise Votes", DTOutput("party_stats"))
              )
      ),
      
      tabItem(tabName = "prob_ci",
              fluidRow(
                box(width = 6, title = "Probability Distribution of Votes", plotlyOutput("vote_distribution")),
                box(width = 6, title = "Confidence Intervals for Votes", verbatimTextOutput("ci_results"))
              )
      ),
      
      tabItem(tabName = "regression",
              fluidRow(
                box(width = 6, title = "Linear Regression: Population vs Votes", plotOutput("regression_plot")),
                box(width = 6, title = "Predict Votes",
                    numericInput("pop_input", "Population:", value = 700000, min = 0),
                    numericInput("total_input", "Registered Voters:", value = 350000, min = 0),
                    numericInput("polling_input", "Polling Stations:", value = 300, min = 0),
                    numericInput("gender_ratio_input", "Gender Ratio (M/F):", value = 1.25, min = 0),
                    actionButton("predict_btn", "Predict"),
                    verbatimTextOutput("prediction_result")
                )
              )
      )
      
    )
  )
)

#=================================================================================================[ Server ]
server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- elections
    if (input$year_filter != "All") {
      data <- data %>% filter(year_election == input$year_filter)
    }
    data
  })
  #---------------------------------------------------[Data Preview Page]
  output$data_preview <- renderDT({
  df <- elections %>% 
    select(-geom, -party_affilation) %>% 
    rename(
      Province = province,
      District = district,
      Constituency = constituency_name,
      Population = pop,
      `Polling Stations` = no_of_polling_stations,
      `Male Voters` = male,
      `Female Voters` = female,
      `Total Voters` = total,
      `Constituency Code (NA)` = na_name,
      `Votes Obtained` = obtained_votes,
      `Candidate Name` = candidate_name,
      `Party` = party,
      `Year` = year_election
    )
  
  datatable(df, options = list(scrollX = TRUE))
})
  
  output$votes_party <- renderPlotly({
    filtered_data() %>%
      group_by(party) %>%
      summarise(total_votes = sum(obtained_votes, na.rm = TRUE)) %>%
      plot_ly(x = ~party, y = ~total_votes, type = 'bar', marker = list(color = '#FF5733')) %>%
      layout(
        title = "Votes by Party",
        xaxis = list(title = "Party", tickangle = -45),
        yaxis = list(title = "Total Votes"),
        plot_bgcolor = '#1e1e2f',
        paper_bgcolor = '#1e1e2f',
        font = list(color = '#ffffff')
      )
  })
  
  output$votes_year <- renderPlotly({
    filtered_data() %>%
      group_by(year_election) %>%
      summarise(total_votes = sum(obtained_votes, na.rm = TRUE)) %>%
      plot_ly(x = ~year_election, y = ~total_votes, type = 'scatter', mode = 'lines+markers', line = list(color = '#1E90FF')) %>%
      layout(
        title = "Votes Over Years",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Votes"),
        plot_bgcolor = '#1e1e2f',
        paper_bgcolor = '#1e1e2f',
        font = list(color = '#ffffff')
      )
  })
  
  output$polling_province <- renderPlotly({
    filtered_data() %>%
      group_by(province) %>%
      summarise(total_polling = sum(no_of_polling_stations, na.rm = TRUE)) %>%
      plot_ly(x = ~province, y = ~total_polling, type = 'bar', marker = list(color = '#32CD32')) %>%
      layout(
        title = "Polling Stations by Province",
        xaxis = list(title = "Province"),
        yaxis = list(title = "Polling Stations"),
        plot_bgcolor = '#1e1e2f',
        paper_bgcolor = '#1e1e2f',
        font = list(color = '#ffffff')
      )
  })
  
  output$gender_distribution <- renderPlotly({
    gender_data <- filtered_data() %>%
      summarise(Male = sum(male, na.rm = TRUE), Female = sum(female, na.rm = TRUE))
    plot_ly(labels = names(gender_data), values = unlist(gender_data), type = 'pie', marker = list(colors = c('#FF6347', '#4682B4'))) %>%
      layout(
        title = "Gender Distribution (Registered Voters)",
        plot_bgcolor = '#1e1e2f',
        paper_bgcolor = '#1e1e2f',
        font = list(color = '#ffffff')
      )
  })
  
  output$turnout_province <- renderPlotly({
    filtered_data() %>%
      group_by(province) %>%
      summarise(total_votes = sum(obtained_votes, na.rm = TRUE),
                total_registered = sum(male + female, na.rm = TRUE)) %>%
      mutate(turnout = total_votes / total_registered * 100) %>%
      plot_ly(x = ~province, y = ~turnout, type = 'bar', marker = list(color = '#FF4500')) %>%
      layout(
        title = "Turnout Percentage by Province",
        xaxis = list(title = "Province"),
        yaxis = list(title = "Turnout %"),
        plot_bgcolor = '#1e1e2f',
        paper_bgcolor = '#1e1e2f',
        font = list(color = '#ffffff')
      )
  })
  
  output$top_parties <- renderPlotly({
    filtered_data() %>%
      group_by(party) %>%
      summarise(total_votes = sum(obtained_votes, na.rm = TRUE)) %>%
      arrange(desc(total_votes)) %>%
      slice_head(n = 10) %>%
      plot_ly(x = ~reorder(party, total_votes), y = ~total_votes, type = 'bar', marker = list(color = '#9B30FF')) %>%
      layout(
        title = "Top 10 Parties by Votes",
        xaxis = list(title = "Party"),
        yaxis = list(title = "Total Votes"),
        plot_bgcolor = '#1e1e2f',
        paper_bgcolor = '#1e1e2f',
        font = list(color = '#ffffff')
      )
  })
  
  output$votes_polling <- renderPlotly({
    filtered_data() %>%
      plot_ly(x = ~no_of_polling_stations, y = ~obtained_votes, type = 'scatter', mode = 'markers', marker = list(color = '#7B68EE')) %>%
      layout(
        title = "Votes vs Polling Stations",
        xaxis = list(title = "No. of Polling Stations"),
        yaxis = list(title = "Votes"),
        plot_bgcolor = '#1e1e2f',
        paper_bgcolor = '#1e1e2f',
        font = list(color = '#ffffff')
      )
  })
  
  # Descriptive Stats - Summary Table---------------------------------------------------------------------------------
  output$summary_table <- renderTable({
    summary_stats <- df_clean %>%
      summarise(
        MeanPop = mean(pop, na.rm = TRUE),
        MedianPop = median(pop, na.rm = TRUE),
        MeanVotes = mean(obtained_votes, na.rm = TRUE),
        MedianVotes = median(obtained_votes, na.rm = TRUE),
        SDVotes = sd(obtained_votes, na.rm = TRUE)
      )
    summary_stats
  })
  
  output$summary_stats <- renderPrint({
    summary(df_clean)
  })
  
  #--------------------
  output$party_stats <- renderDT({
    elections %>%
      group_by(party) %>%
      summarise(
        AvgVotes = mean(obtained_votes, na.rm = TRUE),
        TotalVotes = sum(obtained_votes, na.rm = TRUE),
        Constituencies = n()
      ) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # Descriptive Stats - Boxplot for Obtained Votes
  output$boxplot_votes <- renderPlot({
    ggplot(df_clean, aes(x = as.factor(year_election), y = obtained_votes)) +
      geom_boxplot(fill = "#4682B4", color = "#ffffff") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1e1e2f", color = NA),
        panel.background = element_rect(fill = "#1e1e2f", color = NA),
        axis.text = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff")
      ) +
      labs(title = "Boxplot of Obtained Votes by Year", x = "Year", y = "Obtained Votes")
  })
  
  # Probability Distribution
  output$vote_distribution <- renderPlotly({
    plot_ly(
      x = df_clean$obtained_votes,
      type = 'histogram',
      nbinsx = 20,
      marker = list(
                    color = '#FF6347',
                    line = list(
                      color = '#1e1e2f',  # Border color
                      width = 1           # Border width
                    ))
    ) %>%
      layout(
        title = "Distribution of Obtained Votes",
        xaxis = list(
          title = "Votes",
          zeroline = TRUE,
          showline = TRUE,
          linecolor = '#ffffff',
          linewidth = 2
        ),
        yaxis = list(
          title = "Frequency",
          zeroline = TRUE,
          showline = TRUE,
          linecolor = '#ffffff',
          linewidth = 2
        ),
        plot_bgcolor = '#1e1e2f',
        paper_bgcolor = '#1e1e2f',
        font = list(color = '#ffffff')
      )
  })
  
  output$ci_results <- renderPrint({
    # Example confidence interval for votes
    vote_ci <- t.test(df_clean$obtained_votes)
    vote_ci
  })
  
  # Regression & Prediction - Linear Regression Plot
  output$regression_plot <- renderPlot({
    ggplot(df_clean, aes(x = pop, y = obtained_votes)) +
      geom_point(color = "#FF4500") +
      geom_smooth(method = "lm", color = "#4682B4") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1e1e2f", color = NA),
        panel.background = element_rect(fill = "#1e1e2f", color = NA),
        axis.text = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff")
      ) +
      labs(title = "Linear Regression: Population vs Obtained Votes", x = "Population", y = "Obtained Votes")
  })
  
  # Regression & Prediction - Prediction based on user inputs
  output$prediction_result <- renderPrint({
    req(input$predict_btn)
    
    new_data <- data.frame(
      pop = input$pop_input,
      total = input$total_input,
      no_of_polling_stations = input$polling_input,
      Gender_Ratio = input$gender_ratio_input
    )
    
    prediction <- predict(vote_model, newdata = new_data)
    cat("Predicted Obtained Votes:", round(prediction, 0))
  })
  
}

# UI update to include new outputs for missing pages

ui <- dashboardPage(
  dashboardHeader(title = "Pakistan Elections Analysis (2008–2024)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("table")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Descriptive Stats", tabName = "stats", icon = icon("calculator")),
      menuItem("Probability Distribution", tabName = "prob_ci", icon = icon("chart-area")),
      menuItem("Regression & Prediction", tabName = "regression", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
  /* Global background and text */
  body, .content-wrapper, .right-side {
  background: radial-gradient(circle at bottom, #0b0033, #1a0056, #270067, #0e0e23) !important;
  color: #f0f0f0 !important;
    #background: linear-gradient(135deg, #1c1139, #2a1f4d) !important;
    #color: #f0f0f0 !important;
    font-family: 'Segoe UI', sans-serif;
  }

  /* Only outer containers get transparency */
  .box, .info-box, .small-box, .well, .table-container {
    background: rgba(255, 255, 255, 0.08) !important;
    backdrop-filter: blur(12px) !important;
    -webkit-backdrop-filter: blur(12px) !important;
    border-radius: 15px !important;
    border: 1px solid rgba(255, 255, 255, 0.2) !important;
    color: #ffffff !important;
    box-shadow: 0 8px 32px 0 rgba(31, 38, 135, 0.37);
  }

  /* DO NOT style box-body or tab-content again — avoids layering */
  /* Sidebar and header */
  .main-header .logo, .main-header .navbar, .skin-blue .main-sidebar {
    background-color: rgba(25, 12, 51, 0.9) !important;
    border-bottom: 1px solid #3f3f5f;
  }

  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a,
  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
    background-color: rgba(48, 28, 99, 0.9) !important;
    color: #ffffff !important;
  }

  .sidebar-menu > li > a {
    color: #bbbbff !important;
  }

  /* Table styling */
  .dataTable, .dataTable th, .dataTable td {
    background: transparent !important;
    color: #ffffff !important;
    border: 1px solid rgba(255, 255, 255, 0.2) !important;
  }

  .dataTable th {
    backdrop-filter: blur(8px) !important;
    -webkit-backdrop-filter: blur(8px) !important;
    font-weight: bold;
    background: rgba(255, 255, 255, 0.05) !important;
  }

  /* Inputs, dropdowns, buttons */
  .form-control, .selectize-input, .btn {
    background-color: rgba(255, 255, 255, 0.08) !important;
    color: #ffffff !important;
    border: 1px solid rgba(255, 255, 255, 0.2) !important;
    backdrop-filter: blur(8px) !important;
    -webkit-backdrop-filter: blur(8px) !important;
  }

  .btn:hover {
    background-color: rgba(255, 255, 255, 0.2) !important;
  }

  .box-title {
    color: #e0e0ff !important;
    font-weight: 600;
    backdrop-filter: blur(8px);
    -webkit-backdrop-filter: blur(8px);
  }
  
   /* Lighten text in DataTables controls */
  .dataTables_length label,
  .dataTables_filter label,
  .dataTables_info,
  .dataTables_paginate {
    color: #d0d0d0 !important;
  }

  .box-header {
  background: transparent !important;
  border-bottom: none !important;
  box-shadow: none !important;
  backdrop-filter: none !important;
  -webkit-backdrop-filter: none !important;
}

.box-title {
  background: transparent !important;
  padding: 0 !important;
  color: #e0e0ff !important;
  font-weight: 600;
  backdrop-filter: none !important;
  -web
  
  
"))
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Dataset Preview", width = 12,
                    DTOutput("data_preview"))
              )
      ),
      
      tabItem(tabName = "eda",
              fluidRow(
                box(width = 12, title = "Filters",
                    selectInput("year_filter", "Select Year:",
                                choices = c("All", sort(unique(elections$year_election))),
                                selected = "All")
                )
              ),
              fluidRow(
                box(width = 6, title = "Votes by Party", plotlyOutput("votes_party")),
                box(width = 6, title = "Votes by Year", plotlyOutput("votes_year"))
              ),
              fluidRow(
                box(width = 6, title = "Polling Stations by Province", plotlyOutput("polling_province")),
                box(width = 6, title = "Gender Distribution", plotlyOutput("gender_distribution"))
              ),
              fluidRow(
                box(width = 6, title = "Turnout % by Province", plotlyOutput("turnout_province")),
                box(width = 6, title = "Top 10 Parties by Votes", plotlyOutput("top_parties"))
              ),
              fluidRow(
                box(width = 12, title = "Votes vs Polling Stations", plotlyOutput("votes_polling"))
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("info-circle"), "Summary Statistics"),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  tabsetPanel(
                    tabPanel("Table", tableOutput("summary_table")),
                    tabPanel("Raw Output", verbatimTextOutput("summary_stats")),
                    tabPanel("Boxplot", plotOutput("boxplot_votes"))
                  )
                )
              ),
              fluidRow(
                box(width = 12, title = "Party-wise Votes", DTOutput("party_stats"))
              )
      ),
      
      tabItem(tabName = "prob_ci",
              fluidRow(
                box(width = 6, title = "Probability Distribution of Votes", plotlyOutput("vote_distribution")),
                box(width = 6, title = "Confidence Intervals for Votes", verbatimTextOutput("ci_results"))
              )
      ),
      
      tabItem(tabName = "regression",
              fluidRow(
                box(width = 6, title = "Linear Regression: Population vs Votes", plotOutput("regression_plot")),
                box(width = 6, title = "Predict Votes",
                    numericInput("pop_input", "Population:", value = 700000, min = 0),
                    numericInput("total_input", "Registered Voters:", value = 350000, min = 0),
                    numericInput("polling_input", "Polling Stations:", value = 300, min = 0),
                    numericInput("gender_ratio_input", "Gender Ratio (M/F):", value = 1.25, min = 0),
                    actionButton("predict_btn", "Predict"),
                    verbatimTextOutput("prediction_result")
                )
              )
      )
    )
  )
)

# Run the app
shinyApp(ui, server)