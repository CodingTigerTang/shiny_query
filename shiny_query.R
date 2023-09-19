library(shiny)
library(shinydashboard)
library(readr)
library(DBI)
library(RSQLite)
library(autor)
library(DT)
library(blastula)
library(future)
library(promises)
library(ipc)
library(dplyr)
library(lubridate)
plan(multisession)

# Set the seed for reproducibility
set.seed(123)

# Generate a list of movie theaters and states
movie_theaters <- paste0("Theater", 1:100)
states <- rep(state.name, length.out = 100)

# Generate random data for daily ticket sales
start_date <- ymd("2022-04-01")
end_date <- ymd("2023-08-22")
date_range <- seq(start_date, end_date, by = "day")
num_days <- length(date_range)
num_theaters <- length(movie_theaters)

ticket_sales <- data.frame(
  Date = rep(date_range, times = num_theaters),
  Theater = rep(movie_theaters, each = num_days),
  State = rep(states, each = num_days),
  TicketSales = sample(0:100, num_days * num_theaters, replace = TRUE)
)

# Create a connection to an SQLite database and write the dataset as a table
db_conn <- dbConnect(SQLite(), "test_database.db")
dbWriteTable(db_conn, "sales", ticket_sales %>% 
               mutate(Date = as.character(Date)), overwrite = T)
dbDisconnect(db_conn)

simulated_query_wait <- 10
sender_email <- "<your_email_address>"

# Define UI
ui <- fluidPage(
  titlePanel("Database Information App"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:",min = Sys.Date()-365,max = Sys.Date()),
      selectInput("state", "Select States:", choices = c("All",state.name), multiple =  T),
      textInput("email","Your email:", value = ""),
      actionButton("loadButton", "Load Data"),
      actionButton("loadButton2", "Load Data(future)"),
      br(),
    div(
          h5("Database info: Ticket sales from 2022-04-01 to 2023-08-22 for 100 theaters for states in the US")
    )
    ),
    mainPanel(
      dataTableOutput("tableData"),
      dataTableOutput("tableData2")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Create a connection to the SQLite database
  db_conn <- dbConnect(SQLite(), "test_database.db")
  
  loadedData <- eventReactive(input$loadButton, {
    req(input$state, input$dateRange)
    stateName <- input$state %>% paste0(collapse = "','")
    dateRange <- input$dateRange
    
    if (stateName == "All") {
      query <- glue::glue("SELECT strftime('%Y-%m', Date) as Months, sum(TicketSales) as TicketSales FROM sales WHERE Date BETWEEN '{dateRange[1]}' AND '{dateRange[2]}' group by strftime('%Y-%m', Date)")
    } else {
      query <- glue::glue("SELECT State, strftime('%Y-%m', Date) as Months, sum(TicketSales) as TicketSales FROM sales 
                        WHERE Date BETWEEN '{dateRange[1]}' AND '{dateRange[2]}' 
                        AND State IN ('{stateName}') group by strftime('%Y-%m', Date), State")   
    }
    
    withProgress({
      Sys.sleep(simulated_query_wait)
      data <- dbGetQuery(db_conn, query)
      
      if(nrow(data) >= 1) {
        data <- data %>% 
          janitor::adorn_totals()
      }
      
      if (input$email != "" & sender_email != "<your_email_address>") {
        
        write_csv(data,"data/attachment.csv")
        
        compose_email(
          body = "Hello, attached is the uploaded data file.",
          title = "Data File Attachment"
        ) %>% 
          add_attachment(file = paste0(here::here(),"/data/attachment.csv")) %>%
          smtp_send(
            to = input$email,
            from = sender_email,
            credentials = creds_file("gmail_creds")) # set up your gmail_creds with bastula, run `?bastula::create_smtp_creds_file()`
        
        data
      } else {
        data
      }
      
      
    },message = "Phyllis told me to have a great day, so I took the rest of the day off.")
    
    
  })
  
  # Render the loaded data
  output$tableData <- renderDT({
    loadedData() %>% 
      datatable(.,rownames = FALSE,escape = FALSE,
                extensions = c("Buttons"),
                options = list(dom = 'Bfrtip', buttons = c("csv","excel")),
                class = 'hover cell-border stripe compact nowrap') %>% 
      formatCurrency(c("TicketSales"),currency = "",digits = 0, interval = 3, mark = ",")
    
  })
  
  loadedData2 <- eventReactive(input$loadButton2, {
    req(input$state, input$dateRange)
    stateName <- input$state %>% paste0(collapse = "','")
    dateRange <- input$dateRange
    email <- input$email
    
    if (stateName == "All") {
      query <- glue::glue("SELECT State, strftime('%Y-%m', Date) as Months, sum(TicketSales) as TicketSales FROM sales WHERE Date BETWEEN '{dateRange[1]}' AND '{dateRange[2]}' group by strftime('%Y-%m', Date)")
    } else {
      
      query <- paste0("SELECT State, strftime('%Y-%m', Date) as Months, sum(TicketSales) as TicketSales  FROM sales 
                        WHERE Date BETWEEN '",dateRange[1],"' AND '",dateRange[2],"' 
                        AND State IN ('",stateName,"') group by strftime('%Y-%m', Date), State")
    }
    
    
    future_ops <- function(query,email,sender_email) {
      progress <- AsyncProgress$new(message="<your_msgs>")
      future_promise({
        progress$inc(1/10) 
        db_conn <- dbConnect(SQLite(), "test_database.db")
        
        Sys.sleep(simulated_query_wait)
        data <- DBI::dbGetQuery(db_conn, query)
        
        if(nrow(data) >= 1) {
          data <- data %>% 
            janitor::adorn_totals()
        }
        
        DBI::dbDisconnect(db_conn)
        
        if (email != "" & sender_email != "<your_email_address>") {
          
          write_csv(data,"data/attachment.csv")
          
          compose_email(
            body = "Hello, attached is the uploaded data file.",
            title = "Data File Attachment"
          ) %>% 
            add_attachment(file = paste0(here::here(),"/data/attachment.csv")) %>%
            smtp_send(
              to = email,
              from = sender_email,
              credentials = creds_file("gmail_creds")) # set up your gmail_creds with bastula, run `?bastula::create_smtp_creds_file()`
          
          progress$close() # Close the progress bar
          data
        } else {
          progress$close() # Close the progress bar
          data
        }
      })
    }
    future_ops(query,email,sender_email)
    
  })
  
  
  output$tableData2 <- renderDT({
    loadedData2() %...>% 
      datatable(rownames = FALSE,escape = FALSE,
                extensions = c("Buttons"),
                options = list(dom = 'Bfrtip', buttons = c("csv","excel")),
                class = 'hover cell-border stripe compact nowrap') %...>% 
      formatCurrency(c("TicketSales"),currency = "",digits = 0, interval = 3, mark = ",")
    
  })
  
  
  # output$motivation <- renderText({
  #   HTML(
  #     h1("Empower non-technical users with Shiny"),
  #     h2("In most companies, the data requests will be handled by data teams as the access for databases are limited and most stakeholders do not know how to write SQL code. Thus, data team would get multiple requests that share the SQL structure but not the same parameter. This is where Shiny would come in. We can use Shiny app to serve as the middle man to offer a controlled access to the database and allow users to directly get results that would normally come through as a request.")
  #   )
    
    
 #  })
  
  
  # Close the database connection on app exit
  session$onSessionEnded(function() {
    dbDisconnect(db_conn)
  })
}

# Run the Shiny app
shinyApp(ui, server)
