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

##############################################################
#load data required for ADT graph
patients_df <- tbl(con_bq, "patients") 
transfers_df <- tbl(con_bq, "transfers") 
labevents_df <- tbl(con_bq, "labevents")
procedures_df <- tbl(con_bq, "procedures_icd")
diagnoses_df <- tbl(con_bq, "diagnoses_icd")
d_icd_procedures_df <- tbl(con_bq, "d_icd_procedures")
d_icd_diagnoses_df <- tbl(con_bq, "d_icd_diagnoses")
admissions_df <- tbl(con_bq, "admissions") # Add this for the race info used in the plot
#############################################################
#load data required for ICU graph
icu_all <- tbl(con_bq, "chartevents") |>  
  filter(itemid %in% c(220045, 220179, 223761, 220210)) |>  
  select(subject_id, stay_id, charttime, itemid, valuenum) |>  
  left_join(tbl(con_bq, "d_items") |> select(itemid, abbreviation), by = "itemid") |>  # Join inside BigQuery
  distinct()  # Remove duplicates before collecting
  #mutate(charttime = as.POSIXct(charttime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))


#############################################################
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
        plotOutput("patient_plot") # This should match the output ID in the server
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
 ############################################################################## 
  patient_data <- eventReactive(input$submit_patient, {
    req(input$patient_id)
    
    patient_id <- as.numeric(input$patient_id)
    
    patient_df <- as.data.frame(collect(patients_df |> filter(subject_id == patient_id)))
    transfers_data <- as.data.frame(collect(transfers_df |> filter(subject_id == patient_id))) %>%
      mutate(intime = as.Date(intime), outtime = as.Date(outtime))  # ✅ Convert to Date
    
    lab_data <- as.data.frame(collect(labevents_df |> filter(subject_id == patient_id))) %>%
      mutate(charttime = as.Date(charttime))  # ✅ Convert to Date
    
    procedure_data <- as.data.frame(collect(procedures_df |> filter(subject_id == patient_id))) %>%
      mutate(chartdate = as.Date(chartdate))  # ✅ Convert to Date
    
    admission_data <- as.data.frame(collect(admissions_df |> filter(subject_id == patient_id)))
    diagnoses_data <- as.data.frame(collect(diagnoses_df |> filter(subject_id == patient_id)))
    
    d_icd_procedures_df <- collect(d_icd_procedures_df) |> as.data.frame()
    d_icd_diagnoses_df <- collect(d_icd_diagnoses_df) |> filter(icd_version == "10") |> as.data.frame()
    
    diagnoses_translated <- diagnoses_data |> 
      left_join(d_icd_diagnoses_df, by = "icd_code") |> 
      top_n(3, wt = seq_num) |> as.data.frame()
    
    diagnoses_text <- paste(diagnoses_translated$long_title, collapse = ", ")
    
    procedure_data <- procedure_data |> left_join(d_icd_procedures_df, by = "icd_code")
    
    icu_all <- as.data.frame(collect(icu_all |> filter(subject_id == patient_id)))
    

    list(
      patient_df = patient_df,
      transfers_data = transfers_data,
      lab_data = lab_data,
      procedure_data = procedure_data,
      admission_data = admission_data,
      diagnoses_text = diagnoses_text,
      icu_all = icu_all 
    )
  })
  
  
  output$patient_plot <- renderPlot({
    req(input$plot_type, patient_data())
    
    data <- patient_data()
    
    if (input$plot_type == "ADT") {
      ggplot() +
        # Plot Transfers (Care Units) as segments
        geom_segment(data = data$transfers_data, 
                     aes(x = intime, xend = outtime, 
                         y = "ADT", yend = "ADT", color = careunit),
                     size = 3) +
        
        # Plot Lab Events as points
        geom_point(data = data$lab_data, 
                   aes(x = charttime, y = "Lab"), 
                   shape = 3, size = 2) +
        
        # Plot Procedures as points
        geom_point(data = data$procedure_data, 
                   aes(x = chartdate, y = "Procedure", shape = long_title), 
                   size = 3) +
        
        # Formatting
        labs(
          title = paste0("Patient ", data$patient_df$subject_id[1],
                         ", ", data$patient_df$gender[1], ", ", 
                         data$patient_df$anchor_age[1], " years old, ",
                         ifelse(nrow(data$admission_data) > 0, data$admission_data$race[1], "Race Unknown")),
          subtitle = paste("Diagnoses:", data$diagnoses_text),
          x = "Calendar Time", y = "Type of Event",
          color = "Care Unit",
          shape = "Procedure"
        ) +
        scale_x_date(date_labels = "%b %d") +
        theme_minimal(base_size = 8)
      
    } else if (input$plot_type == "ICU") {
      # **✅ Use `data$icu_all` from patient_data()**
      icu_all <- data$icu_all  
      
      if (nrow(icu_all) == 0) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No ICU data available") + theme_void())
      }
      
      ggplot(icu_all, aes(x = charttime, y = valuenum, color = abbreviation)) +
        geom_line() + 
        geom_point() + 
        facet_grid(abbreviation ~ stay_id, scales = "free") + 
        scale_x_datetime() +
        
        labs(
          title = paste("Patient", input$patient_id, "ICU stays - Vitals"),
          x = "Calendar Time", 
          y = "Vital Value",
          color = "Vital Sign"
        ) +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)
