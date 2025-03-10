library(shiny)
library(tidyverse)
library(bigrquery)
library(gt)

# Authenticate and Connect to BigQuery
satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)
con_bq

#load data required for ADT graph
patients_df <- tbl(con_bq, "patients") 
transfers_df <- tbl(con_bq, "transfers") 
labevents_df <- tbl(con_bq, "labevents")
procedures_df <- tbl(con_bq, "procedures_icd")
diagnoses_df <- tbl(con_bq, "diagnoses_icd")
d_icd_procedures_df <- tbl(con_bq, "d_icd_procedures")
d_icd_diagnoses_df <- tbl(con_bq, "d_icd_diagnoses")

# Load ICU cohort data
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds") %>%
  mutate(
    insurance = as.factor(insurance),
    marital_status = as.factor(marital_status),
    gender = as.factor(gender)
  )

# Define variable groups
variable_groups <- list(
  "Demographics" = c("gender", "race", "insurance", "marital_status", "age_at_intime"),
  "Lab Measurements" = c("bicarbonate", "chloride", "creatinine", "glucose", "potassium", "sodium", "hematocrit", "wbc"),
  "Vitals" = c("heart rate", "non invasive blood pressure systolic", "non invasive blood pressure diastolic", "respiratory rate", "temperature fahrenheit")
)

# UI
ui <- fluidPage(
  titlePanel("ICU Cohort Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tab", "Choose Tab:", choices = c("Summary", "Patient Info")),
      conditionalPanel(
        condition = "input.tab == 'Summary'",
        selectInput("variable_group", "Variable Group", 
                    choices = names(variable_groups)),
        selectizeInput("variable", "Variable", 
                       choices = NULL, multiple = FALSE),
        uiOutput("xlim_ui")
      ),
      conditionalPanel(
        condition = "input.tab == 'Patient Info'",
        textInput("patient_id", "Subject ID:", value = ""),
        actionButton("submit_patient", "Submit"),
        selectInput("plot_type", "Select a plot:", choices = c("ADT", "ICU"))
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.tab == 'Summary'",
        plotOutput("summary_plot"),
        tableOutput("summary_stats")
      ),
      conditionalPanel(
        condition = "input.tab == 'Patient Info'",
        gt_output("patient_info"),
        plotOutput("patient_plot")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$variable_group, {
    updateSelectizeInput(session, "variable", 
                         choices = variable_groups[[input$variable_group]])
  })
  
  output$xlim_ui <- renderUI({
    req(input$variable)
    if (is.numeric(mimic_icu_cohort[[input$variable]])) {
      sliderInput("xlim", "X-axis Range:", 
                  min = min(mimic_icu_cohort[[input$variable]], na.rm = TRUE),
                  max = max(mimic_icu_cohort[[input$variable]], na.rm = TRUE),
                  value = range(mimic_icu_cohort[[input$variable]], na.rm = TRUE))
    }
  })
  
  output$summary_plot <- renderPlot({
    req(input$variable)
    
    data <- mimic_icu_cohort %>%
      filter(!is.na(.data[[input$variable]]))
    
    if (is.numeric(data[[input$variable]])) {
      data <- data %>%
        filter(.data[[input$variable]] >= input$xlim[1] & .data[[input$variable]] <= input$xlim[2])
      
      ggplot(data, aes(x = .data[[input$variable]])) +
        geom_histogram(fill = "steelblue", bins = 30, alpha = 0.7) +
        theme_minimal() +
        labs(x = input$variable, y = "Count", title = paste("Distribution of", input$variable))
      
    } else {
      ggplot(data, aes(x = .data[[input$variable]], fill = .data[[input$variable]])) +
        geom_bar() +
        theme_minimal() +
        labs(x = input$variable, y = "Count", title = paste("Category Distribution of", input$variable)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$summary_stats <- renderTable({
    req(input$variable)
    
    data <- mimic_icu_cohort %>%
      filter(!is.na(.data[[input$variable]]))
    
    if (is.numeric(data[[input$variable]])) {
      summary_values <- summary(data[[input$variable]])
      summary_df <- data.frame(
        Statistic = names(summary_values),
        Value = as.numeric(summary_values)
      )
      summary_df
    } else {
      data %>%
        count(.data[[input$variable]]) %>%
        arrange(desc(n)) %>%
        rename(Category = input$variable, Count = n)
    }
  }, rownames = FALSE)
 
  ##tab2  
  patient_data <- eventReactive(input$submit_patient, {
    req(input$patient_id)  # Ensure input is not empty
    patient_id <- as.numeric(input$patient_id)  # Convert safely
    
    # filter
    patient_df <- patients_df |> filter(subject_id == patient_id) |> collect()
    transfers_df <- transfers_df |> filter(subject_id == patient_id) |> collect()
    labevents_df <- labevents_df |> filter(subject_id == patient_id) |> collect()
    procedures_df <- procedures_df |> filter(subject_id == patient_id) |> collect()
    diagnoses_df <- diagnoses_df |> filter(subject_id == patient_id) |> collect()

# merge & modify date format
    transfers_data <- transfers_df |>
      mutate(intime = as.Date(intime), outtime = as.Date(outtime))
    
    lab_data <- labevents_pq |>
      mutate(charttime = as.Date(charttime))
    
    procedure_data <- procedures_icd_df |>
      mutate(chartdate = as.Date(chartdate))
    
    diagnoses_translated <- diagnoses_icd_df |>
      left_join(d_icd_diagnoses_df, by = "icd_code") |>
      filter(icd_version.y == "10")  |>
      top_n(3, wt = seq_num)  # Select top 3 diagnoses
    
    # Convert to a readable format
    diagnoses_text <- paste(diagnoses_translated$long_title, 
                            collapse = ", ")
    
    # Merge procedures with descriptions
    procedures_translated <- procedure_data  |>
      left_join(d_icd_procedures_df, by = "icd_code")
    
    # Ensure procedure titles are available
    procedure_data <- procedure_data  |>
      left_join(d_icd_procedures_df, by = "icd_code") 
  })
  
  output$summary_plot <- renderPlot({
    req(input$plot_type)
    
    if (input$plot_type == "ADT") {
      # Plot ADT
      ggplot() +
        # Plot Transfers (Care Units) as segments
        geom_segment(data = transfers_data, 
                     aes(x = intime, xend = outtime, 
                         y = "ADT", yend = "ADT", color = careunit),
                     size = 3) +
        
        # Plot Lab Events as points
        geom_point(data = lab_data, 
                   aes(x = charttime, y = "Lab"), 
                   shape = 3, size = 2) +
        
        # Plot Procedures as points with different shapes
        geom_point(data = procedure_data, 
                   aes(x = chartdate, y = "Procedure", shape = long_title), 
                   size = 3, fill = "black") +
        
        # Formatting
        labs(
          title = paste0("Patient ", patients_df$subject_id,
                         ", ", patients_df$gender, ", ", 
                         patients_df$anchor_age, " years old, ",
                         admissions_df$race),
          subtitle = paste("Diagnoses:", 
                           paste(diagnoses_text, collapse="\n")),
          x = "Calendar Time", y = "Type of Event",
          color = "Care Unit",
          shape = "Procedure"
        ) +
        scale_x_date(date_labels = "%b %d") +
        theme_minimal(base_size=8) 
    } 
  } )
}
shinyApp(ui, server)