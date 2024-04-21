
library(shinyDarkmode)
library(dplyr)
library(countrycode)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(shinythemes)
library(DT)
library(scales)
library(shinyjs)
library(shinyWidgets)

columns_to_remove <- c("salary", "salary_currency")

# Use select() to remove specified columns
ds_salaries2 <- ds_salaries %>%
  select(-one_of(columns_to_remove))


#converting values in experience column
ds_salaries <- ds_salaries %>%
  mutate(
    experience_level = case_when(
      experience_level == 'EN' ~ 'Entry-level/Junior',
      experience_level == 'MI' ~ 'Mid-level/Intermediate',
      experience_level == 'SE' ~ 'Senior-level/Expert',
      experience_level == 'EX' ~ 'Executive-level/Director',
      TRUE ~ experience_level  # Keep other values unchanged
    )
  )

# Count the occurrences of each experience level
ex_level <- table(ds_salaries$experience_level)

#Average Salary 
average_salary = mean(ds_salaries2$salary_in_usd)
#Average remote ratio 
average_remote = mean(ds_salaries2$remote_ratio)
#Average remote ratio 
frequency_table <- table(ds_salaries2$employment_type)

# Find the value(s) with the highest frequency
mode_values <- names(frequency_table)[frequency_table == max(frequency_table)]

#fillter for calculate growth rate for 2023
ds2023 <- subset(ds_salaries2, work_year == "2023")
ds2022 <- subset(ds_salaries2, work_year == "2022")
growth_rate = ((sum(ds2023$salary_in_usd) - sum(ds2022$salary_in_usd))/sum(ds2022$salary_in_usd)) * 100

#converting values in employment type column
ds_salaries <- ds_salaries %>%
  mutate(
    employment_type = case_when(
      employment_type == 'FT' ~ 'Full-Time',
      employment_type == 'FL' ~ 'FreeLancing',
      employment_type == 'CT' ~ 'Contract',
      employment_type == 'PT' ~ 'Part-Time',
      TRUE ~ employment_type  # Keep other values unchanged
    )
  )


ds_salaries2<-ds_salaries

columns_to_remove <- c("salary", "salary_currency")

# Use select() to remove specified columns
ds_salaries2 <- ds_salaries2 %>%
  select(-one_of(columns_to_remove))




#converting values in employment type column
ds_salaries2 <- ds_salaries2 %>%
  mutate(
    company_size = case_when(
      company_size == 'L' ~ 'Large',
      company_size == 'M' ~ 'Medium',
      company_size == 'S' ~ 'Small',
      TRUE ~ company_size  # Keep other values unchanged
    )
  )
# Use group_by and summarize to get counts for each job_title
job_title_counts <- ds_salaries2 %>%
  group_by(job_title) %>%
  summarize(count = n())

# Arrange the data in descending order based on count
job_title_counts <- job_title_counts %>%
  arrange(desc(count))

# Extract the top 10 job titles
top_10_job_titles <- head(job_title_counts$job_title, 10)



minmax_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#Apply Min-Max scaling to 'your_column'
scaled_values <- minmax_scale(ds_salaries2$remote_ratio)

#Replace specific values with random values
set.seed(123)  # Set seed for reproducibility
ds_salaries2$remote_ratio <- ifelse(ds_salaries2$remote_ratio == 0, runif(length(scaled_values), 0.1, 0.4),
                                    ifelse(ds_salaries2$remote_ratio == 50, 0.5,
                                           ifelse(ds_salaries2$remote_ratio == 100, runif(length(scaled_values), 0.6, 0.9),
                                                  scaled_values)))


ui <- dashboardPage(
  dashboardHeader(
    title = "Data Science Salaries 2023"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("DataSet", icon = icon("table"), tabName = "table"),
      menuItem("Filters","filters",icon = icon("bolt"),
               selectInput("work_year", "Filter by Work Year:", choices = unique(ds_salaries2$work_year), multiple = TRUE),
               selectInput("experience_level", "Filter by Experience Level:", choices = unique(ds_salaries2$experience_level), multiple = TRUE),
               selectInput("employment_type", "Filter by Employment Type:", choices = unique(ds_salaries2$employment_type), multiple = TRUE),
               selectInput("job_title", "Filter by Job Title:", choices = unique(ds_salaries2$job_title), multiple = TRUE),
               selectInput("company_size2", "Filter by Company Size:", choices = unique(ds_salaries2$company_size), multiple = TRUE),
               selectInput("employee_residence", "Filter by Employee Residence:", choices = unique(ds_salaries2$employee_residence), multiple = TRUE),
               selectInput("company_location", "Filter by Company Location:", choices = unique(ds_salaries2$company_location), multiple = TRUE),
               selectInput("unit", "Select Unit", choices = c("Absolute Values", "Percentage Change")) ),
      menuItem("Show/Hide Charts","showplots",icon = icon("eye-slash"),
               checkboxInput("show_histogram", "Show histogram", value = TRUE),
               checkboxInput("show_boxplot", "Show boxplot", value = TRUE),
               checkboxInput("show_barplot", "Show barplot", value = TRUE),
               checkboxInput("show_piechart", "Show  piechart", value = TRUE),
               checkboxInput("show_linechart", "Show linechart", value = TRUE),
               checkboxInput("show_stackedchart", "Show stackedchart", value = TRUE),
               checkboxInput("show_densityplot", "Show densityplot", value = TRUE),
               checkboxInput("show_scatterplot", "Show scatterplot", value = TRUE)),
      menuItem("Themes","themes",icon = icon("moon"),
               prettySwitch("togglemode", "Dark Mode", value = FALSE, fill = TRUE, status = "info")
               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidPage(
                fluidRow(
                  infoBox("Average Salary",
                          fill = TRUE,
                          sprintf("%s $",format(round(average_salary,2), big.mark = ",")),
                          icon = icon("sack-dollar"),
                          color = "green",
                          width = 3
                  ),
                  infoBox("Growth  Rate 2023",
                          sprintf("%.1f%%",round(growth_rate,1)),
                          fill = TRUE,
                          icon = icon("chart-line"),
                          color = "green",
                          width = 3
                  ),
                  infoBox("Average Remote Ratio",
                          fill = TRUE,
                          sprintf("%.2f%%",round(average_remote,2)),
                          icon = icon("home"),
                          color = "green",
                          width = 3
                  ),
                  infoBox("Top Employment Type",
                          fill = TRUE,
                            sprintf("%s ",mode_values),
                          icon = icon("user-tie"),
                          color = "green",
                          width = 3
                  ),
                  use_darkmode()
                  
                ),
                conditionalPanel( 
                  condition = "input.show_histogram == true",
                  
                  fluidRow(
                    box(
                      title = "Select Bins for Histogram",
                      status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      selectInput("bins", "Number of bins:", choices=c(20,40,60,80,100), selected=20)
                    )
                    ,
                    box(
                      title = "Histogram",
                      status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      plotlyOutput("histogram")
                    )
                  )
                )
                ,
                conditionalPanel(  
                  condition = "input.show_boxplot == true",
                  fluidRow(
                    box(
                      title = "Boxplot",
                      status = "success", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      plotlyOutput("boxplot")
                    ),
                    box(
                      title = "Select The Employment Type",
                      status = "success", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      checkboxGroupInput("employee_type", "Choose Employee Type:",
                                         choices = c("Part-Time", "Full-Time", "Contract","FreeLancing"),
                                         selected = c("Part-Time", "Full-Time", "Contract","FreeLancing"))
                    ))
                ),conditionalPanel( 
                  condition = "input.show_barplot == true",
                  
                  fluidRow(
                    box(
                      title = "Select Method",
                      status = "warning", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      radioButtons("Method", "Select Method",
                                   choices = c("Sum", "Average", "Minimum","Maximum"),
                                   selected = c("Sum"))
                    ),
                    box(
                      title = "Barplot",
                      status = "warning", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      plotlyOutput("barplot")
                    ))),
                conditionalPanel(   
                  condition = "input.show_piechart == true",
                  
                  fluidRow(
                    box(title = "Pie Chart",
                        status = "danger",solidHeader = TRUE,
                        collapsible = TRUE,collapsed = TRUE,
                        plotlyOutput("pie_chart")
                    ),
                    box(title = "Select Experience Level",
                        status = "danger",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        selectInput("selected_experience", "Select Experience Level:", choices = unique(ds_salaries2$experience_level), selected = NULL)
                    )
                    
                  ))
                ,
                conditionalPanel(
                  condition = "input.show_linechart == true",
                  
                  fluidRow(
                    box(title = "Select Year Range",
                        status = "info",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        sliderInput("year_range", "Select Year Range", min = 2020, max = 2023, value = c(2022, 2023), step = 1)
                    ),
                    box(title = "Line Chart",
                        status = "info",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        plotlyOutput("linechart")
                        
                    )
                  ))
                ,
                conditionalPanel(  
                  condition = "input.show_stackedchart == true",
                  
                  fluidRow(
                    box(title = "Stacked Chart",
                        status = "primary",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        plotlyOutput("stackedchart")
                        
                    ),
                    box(title = "Choose Company Size:",
                        status = "primary",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        checkboxGroupInput("company_size", "Choose Company Size:",
                                           choices = c("Large", "Medium", "Small"),
                                           selected = c("Large", "Medium", "Small"))
                    )
                  ))
                ,
                conditionalPanel(
                  condition = "input.show_densityplot == true",
                  
                  fluidRow(
                    box
                    (
                      title = "Select Employment Tybe",
                      status = "success", 
                      solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      selectInput("employment_type1", "Filter by Employment Type:", 
                                  choices = unique(ds_salaries2$employment_type), 
                                  multiple = TRUE, 
                                  selected = "Full-Time")       
                    ),
                    box(
                      title = "Density Plot",
                      status = "success", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      plotlyOutput("densityplot")
                    )
                  ))                
                ,
                
                conditionalPanel(
                  condition = "input.show_scatterplot == true",
                  
                  fluidRow(
                    box(
                      title = "Switch ",
                      status = "warning", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      materialSwitch("show_below_05", "Show remote_ratio < 0.5", value = FALSE),
                      materialSwitch("show_above_eq_05", "Show remote_ratio >= 0.5", value = FALSE),
                    ),
                    box(
                      title = "Scatter Plot",
                      status = "warning", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      plotlyOutput("scatterplot"))
                  ))
                
              )
              
              
      ),
      
      tabItem(tabName = "table",
              fluidPage(
                DTOutput("mytable")
              )
      )
    )
    
  )
)

server <- function(input, output) {
  output$histogram <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(x = salary_in_usd)) +
        geom_histogram(bins = as.numeric(input$bins), fill = "lightblue", col = "darkblue") +
        labs(
          title = "Salary Histogram",
          x = "Salary",
          y = "Frequency"
        ) +
        scale_x_continuous(labels = scales::comma)  # Add this line to format labels with commas
    )%>%
      config(displayModeBar = FALSE)
    
  })
  
  output$boxplot <- renderPlotly({
    # Filter data based on selected employee types
    filtered_data2 <- filtered_data() %>%
      filter(employment_type %in% input$employee_type)
    ggplotly(
      #Create boxplot
      ggplot(filtered_data2, aes(x = employment_type, y = salary_in_usd)) +
        geom_boxplot(fill = "lightblue", color = "darkblue") +
        labs(
          title = "Salary Boxplot by Employee Type",
          x = "Employee Type",
          y = "Salary (USD)"
        )+
        scale_y_continuous(labels = scales::comma)  # Add this line to format labels with commas
    )%>%
      config(displayModeBar = FALSE)
    
  })
  output$barplot <- renderPlotly({
    # Check if Method is NULL and return an empty plot if so
    if (is.null(input$Method)) return(plot(NULL))
    
    # Calculate summary for salary based on selected method
    summary_salary <- switch(input$Method,
                             "Sum" = sum,
                             "Average" = mean,
                             "Minimum" = min,
                             "Maximum" = max)
    
    # Plot using ggplot2 with stat_summary
    ggplotly(
      ggplot(filtered_data(), aes(x = experience_level, y = salary_in_usd)) +
        stat_summary(fun = summary_salary, geom = "bar",fill = "lightblue", col = "darkblue") +
        labs(title = paste("Salary Analysis -", input$Method),
             x = "Experience Level",
             y = "Salary (USD)") +
        scale_y_continuous(labels = scales::comma) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the angle and hjust as needed
    ) %>%
      config(displayModeBar = FALSE)
    
  })
  output$pie_chart <- renderPlotly({
    if (is.null(input$selected_experience)) return(NULL)
    
    filtered_data <- filtered_data() %>%
      filter(experience_level == input$selected_experience)
    
    pie_chart_data <- table(filtered_data$employment_type)
    
    if (length(pie_chart_data) == 0) {
      return(NULL)  # Return NULL if filtered data is empty
    }
    
    percentages <- prop.table(pie_chart_data) * 100
    
    pie_chart <- plot_ly(labels = names(pie_chart_data), values = pie_chart_data, type = "pie") %>%
      add_trace(marker = list(colors = RColorBrewer::brewer.pal(length(pie_chart_data), "Set3")),
                textinfo = "percent+label", 
                hoverinfo = "label+percent",
                textposition = "inside")
    
    pie_chart <- layout(pie_chart, 
                        title = paste("Distribution of Employment Types for", input$selected_experience),
                        titlefont = list(size = 14),  # Set your desired font size
                        autosize = FALSE,
                        width = 450,  # Set your desired width
                        height = 400) # Set your desired height
    
    print(pie_chart)%>%
      config(displayModeBar = FALSE)
  })
  
  
  
  
  output$linechart <- renderPlotly({
    # Filter data based on selected year range
    filtered_data <- filtered_data() %>%
      filter(work_year >= input$year_range[1] & work_year <= input$year_range[2])
    # Calculate the average salary for each year
    avg_salary_by_year <- filtered_data %>%
      group_by(work_year) %>%
      summarise(avg_salary = mean(salary_in_usd))
    # Create line chart
    ggplotly(
      ggplot(avg_salary_by_year, aes(x = work_year, y = avg_salary)) +
        geom_line() +
        labs(
          title = "Average Salary Over the Years",
          x = "Work Year",
          y = "Average Salary (USD)"
        )
    )%>%
      config(displayModeBar = FALSE)
    
  })
  output$stackedchart <- renderPlotly({
    # Subset the original data frame
    subset_data <- filtered_data() %>% 
      filter(job_title %in% top_10_job_titles)
    filtered_data <- subset_data[subset_data$company_size %in% input$company_size, ]
    ggplotly(
      ggplot(filtered_data, aes(x = job_title, fill = company_size)) +
        geom_bar(stat = "count") +  # Adding y aesthetic
        labs(title = "Top 10 Job Titles Across Company Sizes",
             x = "Job Title",
             y = "Count",
             fill = "Company Size") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )%>%
      config(displayModeBar = FALSE)
    
  })
  
  filtered_data <- reactive({
    data <- ds_salaries2
    
    # Apply filters based on user input
    if (!is.null(input$work_year)) {
      data <- data[data$work_year %in% input$work_year, ]
    }
    
    if (!is.null(input$experience_level)) {
      data <- data[data$experience_level %in% input$experience_level, ]
    }
    
    if (!is.null(input$employment_type)) {
      data <- data[data$employment_type %in% input$employment_type, ]
    }
    
    if (!is.null(input$job_title)) {
      data <- data[data$job_title %in% input$job_title, ]
    }
    if (!is.null(input$company_size2)) {
      data <- data[data$company_size2 %in% input$company_size2, ]
    }
    if (!is.null(input$company_location)) {
      data <- data[data$company_location %in% input$company_location, ]
    }
    if (!is.null(input$employee_residence)) {
      data <- data[data$employee_residence %in% input$employee_residence, ]
    }
    if (input$unit == "Percentage Change") {
      data$salary_in_usd <- data$salary_in_usd / sum(data$salary_in_usd) * 100
    }
    
    return(data)
  })
  output$densityplot <- renderPlotly({
    # Check if the selected employment types are NULL and return an empty plot if so
    if (is.null(input$employment_type1) || length(input$employment_type1) == 0) {
      return(plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "Select at least one Employment Type"))
    }
    
    # Filter data based on the selected employment types
    filtered_data <- filtered_data() %>%
      filter(employment_type %in% input$employment_type1)
    
    # Check if the filtered data is not empty
    if (nrow(filtered_data) == 0) {
      # If empty, return a message or an empty plot
      return(plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No data available"))
    }
    
    # Create density plot
    gg_density <- ggplot(filtered_data, aes(x = salary_in_usd, fill = employment_type)) +
      geom_density(alpha = 0.5) +
      labs(
        title = paste("Density Plot of Salaries for Selected Employment Types"),
        x = "Salary (USD)",
        y = "Density"
      ) +
      scale_fill_manual(values = c("Full-Time" = "red", "FreeLancing" = "blue", "Contract" = "green", "Part-Time" = "orange")) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_continuous(labels = scales::comma)
    
    # Adjust title font size and alignment
    gg_density <- gg_density + theme(plot.title = element_text(size = 10, hjust = 0))  # Adjust the size as needed
    
    # Convert ggplot to plotly
    ggplotly(gg_density) %>%
      config(displayModeBar = FALSE)
  })
  
  
  output$scatterplot <- renderPlotly({
    # Filter data based on toggle switch values
    filtered_data <- filtered_data() %>%
      filter(
        (!input$show_below_05 | (input$show_below_05 & remote_ratio < 0.5)) &
          (!input$show_above_eq_05 | (input$show_above_eq_05 & remote_ratio >= 0.5))
      )
    
    ggplotly(
      ggplot(filtered_data, aes(x = remote_ratio, y = salary_in_usd, label = NULL)) +
        geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.3,color="orange") +
        labs(
          title = "Scatter Plot of Remote Ratio vs. Salary",
          x = "Remote ratio",
          y = "Salary (USD)"
        )+
        scale_y_continuous(labels = scales::comma)
    )%>%
      config(displayModeBar = FALSE)
    
  })
  
  output$mytable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5))
  })
  
  darkmode_toggle(inputid = 'togglemode')
  
}

shinyApp(ui, server)
