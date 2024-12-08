library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(DT)
library(httr)
library(jsonlite)

# Strava API credentials
app_name <- Sys.getenv("STRAVA_APP_NAME")
client_id <- Sys.getenv("STRAVA_CLIENT_ID")
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET")
refresh_token <- Sys.getenv("STRAVA_REFRESH_TOKEN")

# Function to get a fresh access token with improved error handling
get_access_token <- function() {
  token_url <- "https://www.strava.com/oauth/token"
  
  # Validate credentials
  if (any(c(client_id, client_secret, refresh_token) == "")) {
    stop("Missing Strava credentials. Check environment variables.")
  }
  
  # Make the token refresh request
  response <- POST(
    url = token_url,
    body = list(
      client_id = client_id,
      client_secret = client_secret,
      refresh_token = refresh_token,
      grant_type = "refresh_token",
      scope = "activity:read_all,read_all,profile:read_all"  # Added required scopes
    ),
    encode = "form"
  )
  
  # Handle response
  if (status_code(response) != 200) {
    content <- tryCatch(
      fromJSON(rawToChar(response$content)),
      error = function(e) NULL
    )
    error_msg <- if (!is.null(content$message)) content$message else "Unknown error"
    stop(paste("Token refresh failed:", error_msg))
  }
  
  content <- fromJSON(rawToChar(response$content))
  
  if (is.null(content$access_token)) {
    stop("No access token in response")
  }
  
  # Update refresh token if provided
  if (!is.null(content$refresh_token)) {
    refresh_token <<- content$refresh_token
  }
  
  return(content$access_token)
}

# Function to fetch activities with improved error handling
fetch_strava_activities <- function() {
  # Get fresh access token
  access_token <- get_access_token()
  
  url <- "https://www.strava.com/api/v3/athlete/activities"
  
  # Make the API request with updated scopes
  response <- GET(
    url,
    add_headers(
      Authorization = paste("Bearer", access_token),
      Accept = "application/json"
    ),
    query = list(
      per_page = 200,
      access_token = access_token  # Added explicit access token in query
    )
  )
  
  # Handle response
  if (status_code(response) != 200) {
    content <- tryCatch(
      fromJSON(rawToChar(response$content)),
      error = function(e) NULL
    )
    error_msg <- if (!is.null(content$message)) content$message else "Unknown error"
    stop(paste("API request failed:", status_code(response), "Error:", error_msg))
  }
  
  activities <- fromJSON(rawToChar(response$content))
  
  if (length(activities) == 0) {
    return(data.frame())  # Return empty dataframe instead of error
  }
  
  # Create activities dataframe with safer type conversion
  df <- data.frame(
    date = as.Date(activities$start_date),
    type = activities$type,
    country = ifelse(is.null(activities$location_country) | 
                      is.na(activities$location_country) | 
                      activities$location_country == "", 
                    "Unknown", 
                    activities$location_country),
    distance = as.numeric(activities$distance) / 1000,
    duration = as.numeric(activities$moving_time) / 60,
    elevation = as.numeric(activities$total_elevation_gain),
    avg_speed = as.numeric(activities$average_speed) * 3.6,
    avg_watts = as.numeric(activities$average_watts),
    max_watts = as.numeric(activities$max_watts),
    avg_heartrate = as.numeric(activities$average_heartrate),
    max_heartrate = as.numeric(activities$max_heartrate),
    calories = as.numeric(activities$kilojoules)
  )
  
  df %>%
    filter(!type %in% c("Walk", "WeightTraining", "Swim")) %>%
    arrange(desc(date))
}

# Rest of the UI and server code remains the same
ui <- page_sidebar(
  title = "My Strava Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    actionButton("fetch", "Fetch Strava Data", class = "btn-primary"),
    verbatimTextOutput("status"),
    selectInput("activity_type", "Activity Type",
                choices = c("All", "Run", "Ride", "VirtualRide")),
    dateRangeInput("date_range", "Date Range",
                   start = Sys.Date() - 180,
                   end = Sys.Date())
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Distance",
      value = textOutput("total_distance"),
      showcase = bsicons::bs_icon("map"),
      theme = "primary"
    ),
    value_box(
      title = "Total Activities",
      value = textOutput("total_activities"),
      showcase = bsicons::bs_icon("activity"),
      theme = "secondary"
    ),
    value_box(
      title = "Total Elevation",
      value = textOutput("total_elevation"),
      showcase = bsicons::bs_icon("graph-up"),
      theme = "success"
    )
  ),
  
  layout_columns(
    card(
      card_header("Activities Over Time"),
      plotOutput("time_series")
    ),
    card(
      card_header("Distance by Activity Type"),
      plotOutput("activity_dist")
    )
  ),
  
  card(
    card_header("Recent Activities"),
    DTOutput("activity_table")
  )
)

server <- function(input, output, session) {
  activity_data <- reactiveVal(NULL)
  status <- reactiveVal("")
  
  observeEvent(input$fetch, {
    status("Fetching data from Strava...")
    
    tryCatch({
      activities <- fetch_strava_activities()
      activity_data(activities)
      status("Data successfully retrieved!")
    }, error = function(e) {
      status(paste("Error:", conditionMessage(e)))
    })
  })
  
  output$status <- renderText({
    status()
  })
  
  activities <- reactive({
    req(activity_data())
    
    data <- activity_data() %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    
    if (input$activity_type != "All") {
      data <- data %>% filter(type == input$activity_type)
    }
    
    data
  })
  
  output$total_distance <- renderText({
    req(activities())
    paste(round(sum(activities()$distance), 1), "km")
  })
  
  output$total_activities <- renderText({
    req(activities())
    nrow(activities())
  })
  
  output$total_elevation <- renderText({
    req(activities())
    paste(round(sum(activities()$elevation), 0), "m")
  })
  
  output$time_series <- renderPlot({
    req(activities())
    ggplot(activities(), aes(x = date, y = distance, color = type)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE) +
      theme_minimal() +
      labs(x = "Date", y = "Distance (km)", color = "Activity Type") +
      theme(legend.position = "bottom")
  })
  
  output$activity_dist <- renderPlot({
    req(activities())
    activities() %>%
      group_by(type) %>%
      summarise(total_distance = sum(distance)) %>%
      ggplot(aes(x = reorder(type, -total_distance), y = total_distance, fill = type)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Activity Type", y = "Total Distance (km)") +
      theme(legend.position = "none")
  })
  
  output$activity_table <- renderDT({
    req(activities())
    activities() %>%
      arrange(desc(date)) %>%
      select(date, type, country, distance, duration, elevation, 
             avg_speed, avg_watts, max_watts, 
             avg_heartrate, max_heartrate, calories) %>%
      mutate(
        across(where(is.numeric), ~ifelse(is.na(.), NA, round(., 1)))
      ) %>%
      datatable(
        options = list(
          pageLength = 5,
          columnDefs = list(
            list(
              targets = c(7:12),
              render = JS(
                "function(data, type, row, meta) {
                return data === null ? 'N/A' : data;
              }"
              )
            )
          )
        )
      )
  })
}

shinyApp(ui, server)
